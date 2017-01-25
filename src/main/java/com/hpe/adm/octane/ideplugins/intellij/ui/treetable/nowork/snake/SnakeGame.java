package com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.snake;

import com.intellij.util.ImageLoader;
import com.intellij.util.containers.HashSet;

import javax.swing.*;
import javax.swing.Timer;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.*;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class SnakeGame extends JPanel {

	private static final long serialVersionUID = 1L;

	//Number of positions available vertically and horizontally
	private static int verticalPosCount = 10;
	private static int horizontalPosCount = 20;
	private Timer gameLoopTimer;
	private static final Color octaneGreen = new Color(0, 179, 141);
	private static final Color octaneGray = new Color(133, 142, 132);

	private enum GameState {
		NOT_STARTED, RUNNING, PAUSED, OVER, WON;
	}

	private GameState gameState = GameState.NOT_STARTED;

	//state of the game
	private static class SpritePos {
		public int x; //0 <= x < verticalPosCount
		public int y; //0 <= x < horizontalPosCount
		public SpriteDirection dir = SpriteDirection.RIGHT;
		
		public SpritePos(int x, int y) {
			this.x = x;
			this.y = y;
		}
		public SpritePos(int x, int y, SpriteDirection dir) {
			this.x = x;
			this.y = y;
			this.dir = dir;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + x;
			result = prime * result + y;
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			SpritePos other = (SpritePos) obj;
			if (x != other.x)
				return false;
			if (y != other.y)
				return false;
			return true;
		}		
	}

	private enum SpriteDirection {
		UP, DOWN, LEFT, RIGHT;
	}

	private static final int INIT_SIZE = 3;

	//Represents the body of the snake, where the first element is the head
	private List<SpritePos> snakeBody = new ArrayList<>();
	private Deque<SpriteDirection> snakeDirectionQueue = new ArrayDeque<>(4);
	private SpritePos applePos = null;

	private static final int INIT_SPEED = 200;
	private int speed = INIT_SPEED;

	//changes to a random color very apple hit
	private Color backgroundColor = octaneGreen;

	public SnakeGame() {
		requestFocusInWindow();
		setKeyBindings();
		setBorder(null);

		addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
                if(!hasFocus()) {
                    requestFocus();
                }
                //if the game is not started init the snake
                if (GameState.NOT_STARTED.equals(gameState) || GameState.OVER.equals(gameState)) {
                    initGame();
                    gameState = GameState.RUNNING;
                    gameLoopTimer.start();
                } else if (GameState.PAUSED.equals(gameState)) {
                    gameState = GameState.RUNNING;
                    repaint();
                } else if (GameState.RUNNING.equals(gameState)) {
                    gameState = GameState.PAUSED;
                    repaint();
                }
			}
		});
	}

	/**
	 * Move snake in the current direction
	 */
	private void moveSnake(){

		SpriteDirection snakeDirection = snakeDirectionQueue.size() == 1 ? snakeDirectionQueue.peek() : snakeDirectionQueue.poll();

		//Move the head in the current direction
		SpritePos head = snakeBody.get(0);
		int newX = head.x;
		int newY = head.y;
		switch (snakeDirection) {
		case UP:
			newY -= 1;
			break;
		case DOWN:
			newY += 1;
			break;
		case LEFT:
			newX -= 1;
			break;
		case RIGHT:
			newX += 1;
			break;
		}

		//don't move if it's off the board
		if(newX < 0){
			newX = horizontalPosCount - 1;
		} else  if (newX >= horizontalPosCount){
			newX = 0;
		}
		if(newY < 0){
			newY = verticalPosCount - 1;
		} else  if (newY >= verticalPosCount){
			newY = 0;
		}

		SpritePos newHead = new SpritePos(newX, newY);
		newHead.dir = snakeDirection;
		//Check if head is on the apple
		if(newHead.equals(applePos)){
			snakeBody.add(0, new SpritePos(newX, newY, snakeDirection));
			placeRandomApple();
			//get new random background color
			backgroundColor = new Color((int)(Math.random() * 0x1000000));
			speed = (speed > 100) ? (speed - 5) : 100;
			gameLoopTimer.setDelay(speed);
		} else {
			//check in new head collides with the snake body
			for(int i = 1; i<snakeBody.size(); i++){
				SpritePos curr = snakeBody.get(i);
				if(curr.equals(newHead)){
					gameLoopTimer.stop();
					gameState = GameState.OVER;
                    backgroundColor = Color.RED;
					return;
				}
			}
		}


		//Shift the body onto the top of the head
		for(int i = snakeBody.size() - 1; i>0; i--){
			SpritePos curr = snakeBody.get(i);
			SpritePos prev = snakeBody.get(i-1);
			curr.x = prev.x;
			curr.y = prev.y;
			curr.dir = prev.dir;
			snakeBody.set(i, curr);
		}

		//create a new head in the new direction
		snakeBody.set(0, newHead);
	}

	/**
	 * This sets the game state to WON if there is not more space to put an apple
	 */
	private void placeRandomApple(){
		Set<SpritePos> possiblePositions = new HashSet<>();
		for(int x = 0; x<horizontalPosCount; x++){
			for(int y = 0; y<verticalPosCount; y++){
                SpritePos pos = new SpritePos(x,y);
                if(!snakeBody.contains(pos)){
                    possiblePositions.add(pos);
                }
			}
		}

		if(possiblePositions.size() <= 1){
            applePos = null;
            gameState = GameState.WON; //nice
            backgroundColor = octaneGreen;
            return;
        }

		//Pick a random element of the set
        int index = ThreadLocalRandom.current().nextInt(0, possiblePositions.size());
        Iterator<SpritePos> iter = possiblePositions.iterator();
        for (int i = 0; i < index; i++) {
            iter.next();
        }
        applePos = iter.next();
	}

	private void initGame(){
		speed = INIT_SPEED;
		snakeBody.clear();

		int initX = horizontalPosCount / 2 - INIT_SIZE;
		int initY = verticalPosCount / 2;

		//start snake horizontally at the middle
		for(int i = 0; i < INIT_SIZE; i++){
			snakeBody.add(new SpritePos(initX-i,initY));
		}

		snakeDirectionQueue.clear();
		snakeDirectionQueue.push(SpriteDirection.RIGHT);
		placeRandomApple();

		if(gameLoopTimer==null){
			gameLoopTimer = new Timer(speed, e -> {
				if(GameState.RUNNING.equals(gameState)){
					gameState = GameState.RUNNING;
					moveSnake();
					repaint();
				}
			});
            gameLoopTimer.setInitialDelay(0);
		} else {
			gameLoopTimer.setDelay(speed);
		}
	}

    /**
     * Calls the timer action instantly ignoring the delay
     */
    private void callTimerAction(){
        int delay = gameLoopTimer.getDelay();
        gameLoopTimer.stop();
        gameLoopTimer.setDelay(0);
        gameLoopTimer.start();
        gameLoopTimer.stop();
        gameLoopTimer.setDelay(delay);
        gameLoopTimer.start();
    }

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		doDrawing(g);
	}

	private void doDrawing(Graphics g) {
		//Minimum padding
		int padding = 20;

		//Screen size
		int screenWidth = getWidth() - padding; //mandatory padding
		int screenHeight = getHeight() - padding; //mandatory padding

		//determine sprite size from current window size
		int spriteSizeWidth = screenWidth / horizontalPosCount;
		int spriteSizeHeight = screenHeight / verticalPosCount;
		int spriteSize = spriteSizeHeight > spriteSizeWidth ? spriteSizeWidth : spriteSizeHeight;

		//view start
		int x1 = (screenWidth - (spriteSize * horizontalPosCount)) / 2 + (padding/2);
		int y1 = (screenHeight - (spriteSize * verticalPosCount)) / 2 + (padding/2);

		//Background color of the border of the game
		g.setColor(backgroundColor);
		g.fillRect(0, 0,  getWidth(), getHeight());

		//Draw the bounds of the game, the board on which the snake moves
		g.setColor(Color.BLACK);
		g.drawRect(x1 - 2, y1 - 2, horizontalPosCount*spriteSize + 2 , verticalPosCount*spriteSize + 2);
		g.drawRect(x1 - 1, y1 - 1, horizontalPosCount*spriteSize + 2, verticalPosCount*spriteSize + 2);

		//Set the board to a while bg
		g.setColor(Color.WHITE);
		g.fillRect(x1, y1, horizontalPosCount*spriteSize, verticalPosCount*spriteSize);
		
		//Now draw something on the board depending on the game state
		if(GameState.NOT_STARTED.equals(gameState)){
			drawGameStart(g, x1, y1, horizontalPosCount*spriteSize, verticalPosCount*spriteSize);
			
		} else if(GameState.PAUSED.equals(gameState)){
			drawGamePaused(g, x1, y1, horizontalPosCount*spriteSize, verticalPosCount*spriteSize);
			
		} else if(GameState.RUNNING.equals(gameState)){
			
			//Draw the snake according to the game state
			//draw apple
			if(applePos != null){
				g.drawImage(getSprite(SpriteDirection.UP, true), x1 + spriteSize * applePos.x, y1 + spriteSize * applePos.y, spriteSize, spriteSize, this);
			}

			//draw the snake at it's current position
			for(int i = 0; i<snakeBody.size(); i++){
				SpritePos pos = snakeBody.get(i);
				SpritePos prevPos = i>0 ? snakeBody.get(i-1) : null;
				Image sprite;
				if(i == 0){
					sprite = getSprite(pos.dir, true);
				} else if (prevPos != null && !pos.dir.equals(prevPos.dir)){
					sprite = getSprite(null, false);
				} else {
					sprite = getSprite(pos.dir, false);
				}
				if(!pos.equals(prevPos)){
					g.drawImage(sprite, x1 + spriteSize * pos.x, y1 + spriteSize * pos.y, spriteSize, spriteSize, this);
				}
			}
			
			
		} else if(GameState.OVER.equals(gameState)){
			drawGameOver(g, x1, y1, horizontalPosCount*spriteSize, verticalPosCount*spriteSize);
			
		} else if(GameState.WON.equals(gameState)){
			drawGameWon(g, x1, y1, horizontalPosCount*spriteSize, verticalPosCount*spriteSize);
		}
	
		Toolkit.getDefaultToolkit().sync();
	}
	
	private void drawGameStart(Graphics g, int x, int y, int width, int height){
		int titleFontSize = 25;
		int bottomFontSize = 15;
		
		Font titleFont = new JLabel().getFont().deriveFont(Font.BOLD | Font.ITALIC).deriveFont((float)titleFontSize);
		g.setColor(octaneGreen);
		FontMetrics fontMetrics =  getFontMetrics(titleFont);
		g.setFont(titleFont);
		String title = "OCTANE SNAKE";
		g.drawString(title, x + (width- fontMetrics.stringWidth(title)) / 2, y + height / 2 - titleFontSize / 2 + 5);
		
		
		Font bottomFont = new JLabel().getFont().deriveFont(Font.BOLD).deriveFont((float)bottomFontSize);
		g.setColor(octaneGray);
		fontMetrics =  getFontMetrics(bottomFont);
		g.setFont(bottomFont);
		String click = "Click to start, space/click to pause";
		g.drawString(click, x + (width - fontMetrics.stringWidth(click)) / 2 , y + height / 2 + 10);
	}
	
	private void drawGamePaused(Graphics g, int x, int y, int width, int height){
		int titleFontSize = 25;
		int bottomFontSize = 15;
		Font titleFont = new JLabel().getFont().deriveFont(Font.BOLD | Font.ITALIC).deriveFont((float)titleFontSize);
		g.setColor(octaneGray);
		FontMetrics fontMetrics =  getFontMetrics(titleFont);
		g.setFont(titleFont);
		String title = "PAUSED";
		g.drawString(title, x + (width- fontMetrics.stringWidth(title)) / 2, y + height / 2 - titleFontSize / 2 + 5);
		
		
		Font bottomFont = new JLabel().getFont().deriveFont(Font.BOLD).deriveFont((float)bottomFontSize);
		g.setColor(octaneGray);
		fontMetrics =  getFontMetrics(bottomFont);
		g.setFont(bottomFont);
		String score = "Score: " + (snakeBody.size() - INIT_SIZE);
		g.drawString(score, x + (width - fontMetrics.stringWidth(score)) / 2 , y + height / 2 + 10);
		
		String click = "Space/click to resume";
		g.drawString(click, x + (width - fontMetrics.stringWidth(click)) / 2 , y + height / 2 + 25);
		
	}
	
	private void drawGameOver(Graphics g, int x, int y, int width, int height){
		int titleFontSize = 25;
		int bottomFontSize = 15;
		Font titleFont = new JLabel().getFont().deriveFont(Font.BOLD | Font.ITALIC).deriveFont((float)titleFontSize);
		g.setColor(Color.RED);
		FontMetrics fontMetrics =  getFontMetrics(titleFont);
		g.setFont(titleFont);
		String title = "GAME OVER";
		g.drawString(title, x + (width- fontMetrics.stringWidth(title)) / 2, y + height / 2 - titleFontSize / 2 + 5);
		
		
		Font bottomFont = new JLabel().getFont().deriveFont(Font.BOLD).deriveFont((float)bottomFontSize);
		g.setColor(octaneGray);
		fontMetrics =  getFontMetrics(bottomFont);
		g.setFont(bottomFont);
		String score = "Score: " + (snakeBody.size() - INIT_SIZE);
		g.drawString(score, x + (width - fontMetrics.stringWidth(score)) / 2 , y + height / 2 + 10);
		
		String click = "Click to restart, or maybe get back to work...";
		g.drawString(click, x + (width - fontMetrics.stringWidth(click)) / 2 , y + height / 2 + 25);
	}
	
	private void drawGameWon(Graphics g, int x, int y, int width, int height){		
		int titleFontSize = 25;
		int bottomFontSize = 15;
		Font titleFont = new JLabel().getFont().deriveFont(Font.BOLD | Font.ITALIC).deriveFont((float)titleFontSize);
		g.setColor(octaneGreen);
		FontMetrics fontMetrics =  getFontMetrics(titleFont);
		g.setFont(titleFont);
		String title = "GAME WON";
		g.drawString(title, x + (width- fontMetrics.stringWidth(title)) / 2, y + height / 2 - titleFontSize / 2 + 5);
		
		Font bottomFont = new JLabel().getFont().deriveFont(Font.BOLD).deriveFont((float)bottomFontSize);
		g.setColor(octaneGray);
		fontMetrics =  getFontMetrics(bottomFont);
		g.setFont(bottomFont);
		String score = "Wow, that's impressive!";
		g.drawString(score, x + (width - fontMetrics.stringWidth(score)) / 2 , y + height / 2 + 10);
		
		String click = "Maybe you should get back to work now...";
		g.drawString(click, x + (width - fontMetrics.stringWidth(click)) / 2 , y + height / 2 + 25);
	}

	private void setKeyBindings() {
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_UP) {
                    changeDirection(SpriteDirection.UP, SpriteDirection.DOWN);
                }
                if (e.getKeyCode() == KeyEvent.VK_LEFT) {
                    changeDirection(SpriteDirection.LEFT, SpriteDirection.RIGHT);
                }
                if (e.getKeyCode() == KeyEvent.VK_DOWN) {
                    changeDirection(SpriteDirection.DOWN, SpriteDirection.UP);
                }
                if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
                    changeDirection(SpriteDirection.RIGHT, SpriteDirection.LEFT);
                }
                if (e.getKeyCode() == KeyEvent.VK_SPACE) {
                    togglePaused();
                }
            }
        });
	}

	private void changeDirection(SpriteDirection direction, SpriteDirection oppositeDir){
        snakeDirectionQueue.clear();
        snakeDirectionQueue.add(direction);
        callTimerAction();
    }

    private void togglePaused(){
        if(GameState.PAUSED.equals(gameState)){
            gameState = GameState.RUNNING;
        } else if (GameState.RUNNING.equals(gameState)){
            gameState = GameState.PAUSED;
        }
        repaint();
    }

	private Image getSprite(SpriteDirection direction, boolean red){
		String spriteName = "/images/snake/octane";
		if(red){
			spriteName += "-red";
		}
		if(direction != null){
			spriteName+= "-" + direction.name().toLowerCase();
		} else {
			spriteName+= "-empty";
		}
		spriteName+= ".png";
		return ImageLoader.loadFromResource(spriteName);
	}

}