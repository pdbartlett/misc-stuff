import greenfoot.*;  // (World, Actor, GreenfootImage, Greenfoot and MouseInfo)

/**
 * Write a description of class Snake here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Snake extends Actor
{
    private boolean done = false;
    
    /**
     * Act - do whatever the Snake wants to do. This method is called whenever
     * the 'Act' or 'Run' button gets pressed in the environment.
     */
    public void act() 
    {
        if (done) return;
        
        if (Greenfoot.getRandomNumber(100) <= 50)
        {
            Paul paul = ((PaulsWorld) getWorld()).getPaul();
            int dx = paul.getX() - getX();
            int dy = paul.getY() - getY();
            if (Math.abs(dx) > Math.abs(dy))
            {
                setLocation(getX() + (int) Math.signum(dx), getY());
            }
            else
            {
                setLocation(getX(), getY() + (int) Math.signum(dy));
            }
        }
        Actor gotPaul = getOneIntersectingObject(Paul.class);
        if (gotPaul != null)
        {
            getWorld().removeObject(gotPaul);
            Greenfoot.playSound("explosion.wav");
            done = true;
        }
    }    
}
