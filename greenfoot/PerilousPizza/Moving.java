import greenfoot.*;  // (World, Actor, GreenfootImage, Greenfoot and MouseInfo)

/**
 * Write a description of class Moving here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public abstract class Moving extends Actor {
    
    public void setLocation(int x, int y) {
        Game game = (Game) getWorld();
        int minX = getImage().getWidth() / 2;
        if (x < minX) {
            x = minX;
        }
        int maxX = game.getWidth() - minX;
        if (x > maxX) {
            x = maxX;
        }
        int minY = getImage().getHeight() / 2;
        if (y < minY) {
            y = minY;
        }
        int maxY = game.getHeight() - Game.PANEL_HEIGHT - minY;
        if (y > maxY) {
            y = maxY;
        }
        super.setLocation(x, y);
    }
}
