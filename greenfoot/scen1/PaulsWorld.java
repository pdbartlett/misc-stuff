import greenfoot.*;  // (World, Actor, GreenfootImage, Greenfoot and MouseInfo)

/**
 * Write a description of class PaulsWorld here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class PaulsWorld extends World
{
    private Paul paul;
    
    /**
     * Constructor for objects of class PaulsWorld.
     */
    public PaulsWorld()
    {    
        // Create a new world with 600x400 cells with a cell size of 1x1 pixels.
        super(800, 640, 1);
        paul = new Paul();
        addObject(paul, getWidth()/2, getHeight()/2);
        addOther(true);
        for (int i = 1; i <= 5; ++i)
        {
            addOther(false);
        }
    }
    
    private void addOther(boolean isSnake)
    {
        addObject(isSnake ? new Snake() : new Target(),
          Greenfoot.getRandomNumber(getWidth()), Greenfoot.getRandomNumber(getHeight()));
    }
    
    public Paul getPaul()
    {
        return paul;
    }
}
