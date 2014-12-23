import greenfoot.*;  // (World, Actor, GreenfootImage, Greenfoot and MouseInfo)

import java.awt.Color;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;

import java.util.ArrayList;
import java.util.List;

/**
 * Write a description of class Game here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class Game extends World {
    
    public static final int PANEL_HEIGHT = 60;

    private Hero hero = null;
    private int score = 0;
    private int numTargetsNeeded = 1;
    private int numBaddiesNeeded = 1;
    
    /**
     * Constructor for objects of class Game.
     * 
     */
    public Game() {    
        super(1000, 640, 1);
        updateScore(0);
    }
    
    private void updateScore(int increment) {
        score += increment;
        
        GreenfootImage bg = getBackground();
        bg.setColor(Color.BLACK);
        bg.fillRect(0, getHeight() - PANEL_HEIGHT, getWidth(), PANEL_HEIGHT);
        
        bg.setColor(Color.WHITE);
        String text = "Score: " + score;
        FontRenderContext frc = bg.getAwtImage().getGraphics().getFontMetrics().getFontRenderContext();
        Rectangle2D bounds = bg.getFont().getStringBounds(text, frc);
        bg.drawString(text, 0, (int) (getHeight() - (PANEL_HEIGHT - bounds.getX()) / 2));
    }
    
    public void act() {
        if (getObjects(Target.class).isEmpty()) {
            // New game or new level.
            if (hero == null) {
                // New game, so create initial objects.
                hero = new Hero();
                addObject(hero, getWidth()/2, getHeight()/2);
                addRandomly(new Obstacle());
            } else {
                // New level so update score and make harder.
                updateScore(5);
                switch (Greenfoot.getRandomNumber(4)) {
                    case 0:
                      numTargetsNeeded++;
                      break;
                    case 1:
                      numBaddiesNeeded++;
                      break;
                    case 2:
                      addRandomly(new Obstacle());
                      break;
                    case 3:
                      for (Baddy baddy : getAll(Baddy.class)) {
                          baddy.increaseSpeed();
                      }
                      break;
                }
            }
            addTargets();
            addBaddiesIfNeeded();
        }
        
        // Now for the "normal" work: collision checking.
        if (hero != null) {
            Actor check;
            List<Actor> baddiesToRemove = new ArrayList<Actor>();
            for (Baddy baddy : getAll(Baddy.class)) {
                check = baddy.collisionCheck(Obstacle.class);
                if (check != null) {
                    Greenfoot.playSound("explosion.wav");
                    updateScore(1);
                    baddiesToRemove.add(baddy);
                }
            }
            removeObjects(baddiesToRemove);
            
            check = hero.collisionCheck(Target.class);
            if (check != null) {
                Greenfoot.playSound("pop.wav");
                updateScore(2);
                removeObject(check);
            }

            check = hero.collisionCheck(Baddy.class);
            if (check == null) {
                check = hero.collisionCheck(Obstacle.class);
            }
            if (check != null) {
                // Game over.
                Greenfoot.playSound("explosion.wav");
                Actor temp = hero;
                hero = null;
                for (Baddy baddy : getAll(Baddy.class)) {
                    baddy.setPrey(null);
                }
                removeObject(temp);
                Greenfoot.stop();
            }
        }
    }
    
    private void addTargets() {
        for (int i = 1; i <= numTargetsNeeded; i++) {
            addRandomly(new Target());
        }
    }
    
    private void addBaddiesIfNeeded() {
        int numExtraBaddiesNeeded = numBaddiesNeeded - getAll(Baddy.class).size();
        for (int i = 1; i <= numExtraBaddiesNeeded; i++) {
            addRandomly(new Baddy(hero));
        }
    }
    
    private void addRandomly(Actor a) {
        addObject(a, Greenfoot.getRandomNumber(getWidth()), Greenfoot.getRandomNumber(getHeight() - PANEL_HEIGHT));
    }
    
    private <T> List<T> getAll(Class<T> klass) {
        return (List<T>) getObjects(klass);
    }
}
