import greenfoot.*;  // (World, Actor, GreenfootImage, Greenfoot and MouseInfo)

/**
 * Write a description of class Paul here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Paul extends Actor
{
    /**
     * Act - do whatever the Paul wants to do. This method is called whenever
     * the 'Act' or 'Run' button gets pressed in the environment.
     */
    public void act() 
    {
        int x = getX();
        int y = getY();
        if (Greenfoot.isKeyDown("up"))         { y--; }
        else if (Greenfoot.isKeyDown("down"))  { y++; }
        else if (Greenfoot.isKeyDown("left"))  { x--; }
        else if (Greenfoot.isKeyDown("right")) { x++; }
        setLocation(x, y);
        
        Actor t = getOneIntersectingObject(Target.class);
        if (t != null)
        {
            getWorld().removeObject(t);
            Greenfoot.playSound("pop.wav");
        }
    }
}
