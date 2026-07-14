#!/usr/bin/env -S uv run --quiet
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "httpx",
# ]
# ///
import sys
import csv
from pathlib import Path
import httpx

def get_faction_data(api_key: str):
    """Fetch faction members and ALL available armory selections using Torn API v1."""
    # Complete list of armory selections supported natively by v1
    selections = "armor,temporary,weapons,utilities,medical,drugs,boosters"
    inv_url = f"https://api.torn.com/faction/?key={api_key}&comment=TornAPI&selections={selections}"
    members_url = f"https://api.torn.com/faction/?key={api_key}&comment=TornAPI&selections=basic"

    print("Fetching faction data from Torn API (All Armory Sections)...")
    try:
        with httpx.Client() as client:
            inv_res = client.get(inv_url)
            mem_res = client.get(members_url)

            inv_res.raise_for_status()
            mem_res.raise_for_status()

            inv_data = inv_res.json()
            mem_data = mem_res.json()

            if "error" in inv_data:
                sys.exit(f"API Error (Inventory): {inv_data['error'].get('error')}")
            if "error" in mem_data:
                sys.exit(f"API Error (Members): {mem_data['error'].get('error')}")

            return inv_data, mem_data
    except httpx.HTTPError as err:
        sys.exit(f"Network error: {err}")

def process_and_export(inv_data, mem_data):
    """Processes aggregated inventory into flat rows and exports them to CSVs."""
    # Map member IDs to Names
    members_map = {}
    if "members" in mem_data and mem_data["members"]:
        for player_id, info in mem_data["members"].items():
            members_map[str(player_id)] = info.get("name", "Unknown")

    loaned_rows = []
    available_rows = []

    # Iterate over all checked and validated v1 armory categories
    categories = ["armor", "temporary", "weapons", "utilities", "medical", "drugs", "boosters"]

    for category in categories:
        items = inv_data.get(category)
        if not items:
            continue

        # Handle both dicts and lists from the API gracefully
        if isinstance(items, dict):
            item_list = list(items.values())
        elif isinstance(items, list):
            item_list = items
        else:
            continue

        for item in item_list:
            if not isinstance(item, dict):
                continue

            name = item.get("name", "Unknown Item")

            # Consumables (medical, drugs, boosters) don't have a 'type' attribute,
            # so we map it cleanly or use a default fallback
            subtype = item.get("type", "Consumable")

            # 1. Process Available Stock
            # For equipment, use native 'available'.
            # For pure consumables, use 'quantity' (since they can't be loaned out, total quantity = available quantity)
            qty_key = "quantity" if category in ["medical", "drugs", "boosters"] else "available"
            try:
                available_qty = int(item.get(qty_key, item.get("available", 0)))
            except (ValueError, TypeError):
                available_qty = 0

            if available_qty > 0:
                available_rows.append({
                    "Item Name": name,
                    "Category": category.capitalize(),
                    "Subtype": subtype,
                    "Count": available_qty,
                    "Location": "Armory"
                })

            # 2. Process Loaned Items (Skip for pure consumables since they cannot be loaned)
            if category in ["medical", "drugs", "boosters"]:
                continue

            loaned_to = item.get("loaned_to", "")
            loanee_ids = []

            if isinstance(loaned_to, list):
                loanee_ids = loaned_to
            elif isinstance(loaned_to, dict):
                loanee_ids = list(loaned_to.values())
            elif isinstance(loaned_to, str) and loaned_to.strip():
                loanee_ids = [idx.strip() for idx in loaned_to.split(",") if idx.strip()]

            # Filter valid numerical IDs
            clean_loanees = []
            for uid in loanee_ids:
                try:
                    parsed = int(uid)
                    if parsed > 0:
                        clean_loanees.append(str(parsed))
                except (ValueError, TypeError):
                    continue

            # Map the clean borrower IDs to names and collapse identical groupings
            loanee_counts = {}
            for uid in clean_loanees:
                loanee_counts[uid] = loanee_counts.get(uid, 0) + 1

            for uid, count in loanee_counts.items():
                if uid in members_map:
                    borrower = f"{members_map[uid]} [{uid}]"
                else:
                    borrower = f"Ex-Member [{uid}]"

                loaned_rows.append({
                    "Item Name": name,
                    "Category": category.capitalize(),
                    "Subtype": subtype,
                    "Count": count,
                    "Loaned To": borrower
                })

    # Write loaned.csv
    loaned_file = Path("loaned.csv")
    with open(loaned_file, mode="w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["Item Name", "Category", "Subtype", "Count", "Loaned To"])
        writer.writeheader()
        writer.writerows(loaned_rows)
    print(f"✅ Exported {len(loaned_rows)} loan record(s) to: {loaned_file.resolve()}")

    # Write available.csv
    available_file = Path("available.csv")
    with open(available_file, mode="w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["Item Name", "Category", "Subtype", "Count", "Location"])
        writer.writeheader()
        writer.writerows(available_rows)
    print(f"✅ Exported {len(available_rows)} available group(s) to: {available_file.resolve()}")

def main():
    if len(sys.argv) < 2:
        print("Usage: uv run fetch_armory.py <YOUR_API_KEY>")
        print("Example: uv run fetch_armory.py abc123XYZkey")
        sys.exit(1)

    api_key = sys.argv[1]
    inv, mem = get_faction_data(api_key)
    process_and_export(inv, mem)

if __name__ == "__main__":
    main()
