import logging
import aiohttp
import discord
from discord.ext import commands
from discord import app_commands
from typing import Optional

logger = logging.getLogger('miscbot.prices')

class Prices(commands.Cog):
    def __init__(self, bot: commands.Bot):
        self.bot = bot
        
        # 1. Static dictionary purely for fuzzy matching
        self.aliases = {
            "bct": "business class ticket",
            "dp": "donator pack",
            "edvd": "erotic dvd",
            "fak": "first aid kit",
            "fhc": "feathery hotel coupon",
            "lbc": "lawyer business card",
            "point": "points",
            "pt": "points",
            "pts": "points",
            "sed": "small explosive device",
            "sfak": "small first aid kit",
            "xan": "xanax",
            "xtc": "ecstacy",
            "zan": "xanax",
        }
        
        # 2. In-memory cache to be populated dynamically on startup
        # Format will be: {"xanax": 206, "donator pack": 283, ...}
        self.item_cache = {}

    async def cog_load(self):
        """Automatically called by discord.py when the cog is loaded."""
        logger.info("Fetching global Torn item list for local ID mapping cache...")
        url = f"https://api.torn.com/torn/?selections=items&key={self.bot.torn_api_key}"
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(url) as resp:
                    data = await resp.json()
                    
                    if "items" in data:
                        for item_id, item_data in data["items"].items():
                            # Store everything lowercase to make lookups easy
                            name = item_data.get("name", "").lower()
                            self.item_cache[name] = item_id
                            
                        logger.info(f"Successfully cached {len(self.item_cache)} Torn items.")
                    else:
                        logger.error(f"Failed to load items: {data.get('error', 'Unknown Error')}")
        except Exception as e:
            logger.error(f"Network error building item cache: {e}")

    @app_commands.command(name="price", description="Check the market value of an item or points")
    @app_commands.describe(
        item="The name of the item (e.g., xanax, points, fhc)",
        quantity="How many you want to buy (defaults to 1)"
    )
    async def price(self, interaction: discord.Interaction, item: str, quantity: Optional[int] = 1):
        logger.info(f"'/price' invoked by {interaction.user} for {quantity}x {item}")
        
        # Clean the input
        query = item.lower().strip()
        
        # 1. Resolve any aliases first
        resolved_name = self.aliases.get(query, query)
        
        # 2. Look up the ID
        if resolved_name == "points":
            target_id = "points"
        elif resolved_name in self.item_cache:
            target_id = self.item_cache[resolved_name]
        else:
            await interaction.response.send_message(
                f"I don't recognize the item '{item}'. Check the spelling or use an alias.", 
                ephemeral=True
            )
            return

        # Defer response so Discord doesn't timeout while waiting for the Torn API
        await interaction.response.defer()

        # 3. Fetch the fresh price data
        try:
            async with aiohttp.ClientSession() as session:
                if target_id == "points":
                    url = f"https://api.torn.com/market/?selections=pointsmarket&key={self.bot.torn_api_key}"
                    async with session.get(url) as resp:
                        data = await resp.json()
                        
                        if "error" in data:
                            await interaction.followup.send(f"API Error: {data['error']['error']}")
                            return
                            
                        points_data = data.get("pointsmarket", {})
                        if not points_data:
                            await interaction.followup.send("Could not retrieve points market data.")
                            return
                            
                        lowest_price = min([listing["cost"] for listing in points_data.values()])
                        unit_name = "Point"
                        
                else:
                    url = f"https://api.torn.com/torn/{target_id}?selections=items&key={self.bot.torn_api_key}"
                    async with session.get(url) as resp:
                        data = await resp.json()
                        
                        if "error" in data:
                            await interaction.followup.send(f"API Error: {data['error']['error']}")
                            return
                            
                        item_info = data.get("items", {}).get(str(target_id), {})
                        lowest_price = item_info.get("market_value", 0)
                        unit_name = item_info.get("name", item.title())

            total_cost = lowest_price * quantity

            reply = (
                f"🛒 **Market check for {quantity}x {unit_name}**\n"
                f"• Unit Price: **${lowest_price:,}**\n"
                f"• Total Cash Needed: **${total_cost:,}**"
            )
            
            await interaction.followup.send(reply)

        except Exception as e:
            logger.error(f"Error fetching price: {e}")
            await interaction.followup.send("Encountered an error while fetching the data from Torn.")

async def setup(bot: commands.Bot):
    await bot.add_cog(Prices(bot))

