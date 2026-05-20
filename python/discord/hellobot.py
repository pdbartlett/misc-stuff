import logging
import discord
from discord.ext import commands
from discord import app_commands

logging.basicConfig(level=logging.INFO, format='%(asctime)s [%(levelname)s] %(name)s: %(message)s')
logger = logging.getLogger('hellobot')

# 1. Create a Cog class dedicated to your commands
class Greetings(commands.Cog):
    def __init__(self, bot: commands.Bot):
        self.bot = bot

    # The @app_commands.command() decorator works natively exactly like this inside a Cog!
    @app_commands.command(name="hello", description="Gives a warm welcome from the bot")
    async def hello(self, interaction: discord.Interaction):
        logger.info(f"'/hello' invoked by {interaction.user}")
        # Now you can cleanly use self.bot to reference shared state or caches
        await interaction.response.send_message(f"Hello {interaction.user.mention}! I am {bot.user} :)")


# 2. Main Bot Orchestrator
class MyBot(commands.Bot):
    def __init__(self):
        # commands.Bot initializes its own CommandTree internally
        super().__init__(command_prefix="!", intents=discord.Intents.default())

    async def setup_hook(self):
        logger.info("Loading Cogs...")
        # Register the Cog to the bot instance
        await self.add_cog(Greetings(self))

        logger.info("Syncing application commands with Discord...")
        await self.tree.sync()
        logger.info("Syncing complete.")

    async def on_ready(self):
        logger.info(f'Logged in as {self.user}')

if __name__ == "__main__":
    with open('.token', 'r') as file:
        token = file.read().strip()

    bot = MyBot()
    bot.run(token, log_handler=None)
