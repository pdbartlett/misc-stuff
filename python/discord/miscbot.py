import logging
import discord
from discord.ext import commands
from discord import app_commands
from typing import Optional

logging.basicConfig(level=logging.INFO, format='%(asctime)s [%(levelname)s] %(name)s: %(message)s')
logger = logging.getLogger('miscbot')

class Info(commands.Cog):
    def __init__(self, bot: commands.Bot):
        self.bot = bot

    @app_commands.command(name="hello", description="Gives a warm welcome from the bot")
    async def hello(self, interaction: discord.Interaction):
        logger.info(f"'/hello' invoked by {interaction.user}")
        await interaction.response.send_message(f"Hello {interaction.user.mention}! I am {bot.user} :)")

    @app_commands.command(name="whichcar", description="Recommends cars for all / particular tracks")
    @app_commands.describe(track="An optional track, otherwise all are shown")
    async def whichcar(self, interaction: discord.Interaction, track: Optional[str] = None):
        '''
        whichcar [track]
        '''
        logger.info(f"'/whichcar' invoked by {interaction.user}")
        car_choices = dict(
                commerce="Edomondo NSX tuned as TL2",
                convict="Mercia SLR tuned as TL3",
                docks="Volt GT tuned as TL3",
                hammerhead="Edomondo NSX tuned as DS2",
                industrial="Edomondo NSX tuned as TS3",
                meltdown="Edomondo NSX tuned as TS3",
                mudpit="Colina Tanproce tuned as DL3",
                parkland="Edomondo NSX tuned as DS3",
                sewage="Edomondo NSX tuned as TL2",
                speedway="Veloria LFA tuned as TL3",
                stone_park="Echo R8 tuned as DS3",
                two_islands="Edomondo NSX tuned as DL3",
                underdog="Edomondo NSX tuned as TL2",
                uptown="Lambrini Torobravo tuned as TL3",
                vector="Edomondo NSX tuned as TS3",
                withdrawal="Veloria LFA tuned as TL3",
                )
        key = track.lower().replace(" ", "_") if track else None
        if key and key in car_choices:
            reply = f'For {track.title()} I would use {car_choices[key]}.'
        else:
            reply = "\n".join([f"{key}: {value}" for key, value in car_choices.items()])
        reply += "\n\nBased on forum post: https://www.torn.com/forums.php#/p=threads&f=61&t=16411662&b=0&a=0&start=0&to=24971759"
        await interaction.response.send_message(reply)


class MyBot(commands.Bot):
    def __init__(self):
        super().__init__(
            command_prefix=commands.when_mentioned_or(),
            intents=discord.Intents.default()
        )

    async def setup_hook(self):
        logger.info("Loading Cogs...")
        await self.add_cog(Info(self))

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

