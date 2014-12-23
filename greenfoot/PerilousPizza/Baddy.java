import greenfoot.*;  // (World, Actor, GreenfootImage, Greenfoot and MouseInfo)

/**
 * Write a description of class Baddy here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Baddy extends Moving {
    
    private Actor prey;
    private int speed = 50;
    
    public Baddy(Actor prey) {
        setPrey(prey);
    }
    
    public void setPrey(Actor prey) {
        this.prey = prey;
    }
    
    /**
     * Act - do whatever the Baddy wants to do. This method is called whenever
     * the 'Act' or 'Run' button gets pressed in the environment.
     */
    public void act() {
        if ((prey != null) && (Greenfoot.getRandomNumber(100) <= speed)) {
            int dx = prey.getX() - getX();
            int dy = prey.getY() - getY();
            if (Math.abs(dx) > Math.abs(dy)) {
                setLocation(getX() + (int) Math.signum(dx), getY());
            } else {
                setLocation(getX(), getY() + (int) Math.signum(dy));
            }
        }
    }
    
    public void increaseSpeed() {
        speed += 3;
    }
    
    public Actor collisionCheck(Class klass) {
        return getOneIntersectingObject(klass);
    }
}
