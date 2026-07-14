#!/usr/bin/env -S uv run --quiet
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "httpx",
# ]
# ///
import sys
import json
import httpx

# Classic OC 1.0 Crime Names (just in case)
OC_1_NAMES = {
    "blackmail",
    "kidnapping",
    "bomb threat",
    "plane hijacking",
    "political kidnapping",
    "takeover",
    "counterfeiting",
    "money train",
    "robbing of a money train"
}

def get_items_map(api_key: str) -> dict:
    """Fetch the master items directory to map item IDs to names."""
    url = f"https://api.torn.com/torn/?selections=items&key={api_key}&comment=TornAPI"
    print("Loading Torn items directory...")
    try:
        response = httpx.get(url)
        response.raise_for_status()
        data = response.json()

        if "error" in data:
            print(f"⚠️ Warning: Failed to load item directory ({data['error'].get('error')}). IDs will show instead of names.")
            return {}

        items = data.get("items", {})
        return {str(item_id): info.get("name", f"Item #{item_id}") for item_id, info in items.items()}
    except Exception as err:
        print(f"⚠️ Warning: Could not resolve item names due to an error ({err}).")
        return {}

def extract_rewards(rewards_data, item_totals: dict):
    """Parses the v2 rewards structure strictly based on the items payload."""
    if not isinstance(rewards_data, dict):
        return

    items_list = rewards_data.get("items")
    if not isinstance(items_list, list):
        return

    # Sifts through [{"id": 66, "quantity": 6}, ...]
    for entry in items_list:
        if isinstance(entry, dict):
            item_id = entry.get("id")
            qty = entry.get("quantity")
            if item_id is not None and qty is not None:
                item_totals[str(item_id)] = item_totals.get(str(item_id), 0) + int(qty)

def fetch_and_total_rewards(api_key: str):
    items_map = get_items_map(api_key)
    item_totals = {}
    total_crimes_counted = 0
    oc_1_encountered = False

    # We will use the exact completed URL structure you provided
    url = f"https://api.torn.com/v2/faction/crimes?cat=completed&limit=20&offset=0&sort=DESC&key={api_key}&comment=TornAPI"

    print("Analyzing completed Organized Crimes...")

    while url and not oc_1_encountered:
        try:
            response = httpx.get(url)
            response.raise_for_status()
            data = response.json()
        except Exception as err:
            sys.exit(f"❌ Network or API error: {err}")

        if "error" in data:
            sys.exit(f"❌ API Error: {data['error'].get('error')}")

        # Explicitly targets the 'crimes' array
        crimes_list = data.get("crimes")

        if not crimes_list or not isinstance(crimes_list, list):
            break

        for crime in crimes_list:
            if not isinstance(crime, dict):
                continue

            crime_name = crime.get("name", "").strip()

            # Stop condition 1: Check if crime lacks 'slots' (meaning it is a legacy OC 1.0 format)
            # Stop condition 2: Check if crime name is in our legacy block list
            if "slots" not in crime or crime_name.lower() in OC_1_NAMES:
                print(f"\n🛑 Stopped! Encountered legacy OC 1.0 crime: '{crime_name}' (ID: {crime.get('id', 'N/A')})")
                oc_1_encountered = True
                break

            # Process successful OC 2.0 crimes
            status = crime.get("status", "")
            if status == "Successful":
                total_crimes_counted += 1
                rewards = crime.get("rewards")
                if rewards:
                    extract_rewards(rewards, item_totals)

        # Walk pagination safely using the next link provided in the API metadata
        metadata = data.get("_metadata", {})
        links = metadata.get("links", {})
        next_link = links.get("next")

        # If there's a next page and we haven't hit an OC 1.0 stop condition, construct the next URL
        if next_link and not oc_1_encountered:
            # We append the key and comment to the next link so it is fully authenticated
            url = f"{next_link}&key={api_key}&comment=TornAPI"
        else:
            url = None

    # Print Summary Results
    print("\n" + "="*55)
    print(f"📊 SUMMARY OF REWARDS (Across {total_crimes_counted} Successful OC 2.0s)")
    print("="*55)

    if not item_totals:
        print("No item rewards found in the analyzed crimes history.")
    else:
        # Sort items by quantity descending
        sorted_rewards = sorted(item_totals.items(), key=lambda x: x[1], reverse=True)
        for item_id, qty in sorted_rewards:
            name = items_map.get(item_id, f"Unknown Item (ID: {item_id})")
            print(f" • {name:<38} x {qty}")

    print("="*55)

def main():
    if len(sys.argv) < 2:
        print("Usage: ./total_oc_rewards.py <YOUR_API_KEY>")
        sys.exit(1)

    api_key = sys.argv[1]
    fetch_and_total_rewards(api_key)

if __name__ == "__main__":
    main()
