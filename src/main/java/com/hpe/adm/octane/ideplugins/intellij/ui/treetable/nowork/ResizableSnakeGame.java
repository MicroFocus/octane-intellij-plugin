package com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork;

import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.intellij.util.ImageLoader;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public class ResizableSnakeGame extends JPanel {

	private static final long serialVersionUID = 1L;

	//Number of positions available vertically and horizontally
	private static int verticalPosCount = 10;
	private static int horizontalPosCount = 20;

	private static final Image BODY_SPRITE = ImageLoader.loadFromResource(Constants.IMG_OCTANE_ICON);
	private static final Image HEAD_SPRITE = ImageLoader.loadFromResource(Constants.IMG_OCTANE_ICON_RED);
	private static final Image APPLE_SPRITE = ImageLoader.loadFromResource(Constants.IMG_OCTANE_ICON_RED);

	private Timer gameLoopTimer;

	private enum GameState {
		NOT_STARTED, RUNNING, PAUSED, OVER;
	}

	private GameState gameState = GameState.NOT_STARTED;

	//state of the game
	private static class SpritePos {
		public int x; //0 <= x < verticalPosCount
		public int y; //0 <= x < horizontalPosCount
		public SpritePos(int x, int y) {
			this.x = x;
			this.y = y;
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

	private enum SnakeDirection {
		UP, DOWN, LEFT, RIGHT;
	}

	private static final int INIT_SIZE = 3;

	//Represents the body of the snake, where the first element is the head
	private List<SpritePos> snakeBody = new ArrayList<>();
	private Deque<SnakeDirection> snakeDirectionQueue = new ArrayDeque<>(4);
	private SpritePos applePos = null;

	private static final int INIT_SPEED = 150;
	private int speed = INIT_SPEED;
	
	//flips between true and false each move()
	private boolean slither = false;

	public ResizableSnakeGame() {
		requestFocusInWindow();
		setKeyBindings();

		addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				//if the game is not started init the snake 
				if(GameState.NOT_STARTED.equals(gameState) || GameState.OVER.equals(gameState)){
					initGame();
					gameState = GameState.RUNNING;
					gameLoopTimer.start();
				}			
			}
		});

	}

	/**
	 * Move snake in the current direction
	 */
	private void moveSnake(){

		SnakeDirection snakeDirection = snakeDirectionQueue.size() == 1 ? snakeDirectionQueue.peek() : snakeDirectionQueue.poll();

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
		if(newX < 0 || newX >= horizontalPosCount){
			return;
		}
		if(newY < 0 || newY >= verticalPosCount){
			return;
		}

		SpritePos newHead = new SpritePos(newX, newY);
		//Check if head is on the apple
		if(newHead.equals(applePos)){
			snakeBody.add(0, new SpritePos(newX, newY));
			placeRandomApple();
		} else {
			//check in new head collides with the snake body
			for(int i = 1; i<snakeBody.size(); i++){
				SpritePos curr = snakeBody.get(i);
				if(curr.equals(newHead)){
					gameLoopTimer.stop();
					gameState = GameState.OVER;
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
			snakeBody.set(i, curr);
		}

		//create a new head in the new direction
		snakeBody.set(0, new SpritePos(newX, newY));
		
		//flip the value
		slither = !slither;
	}

	private void placeRandomApple(){		
		int randX = ThreadLocalRandom.current().nextInt(0, horizontalPosCount);
		int randY = ThreadLocalRandom.current().nextInt(0, verticalPosCount);
		SpritePos candidatePos = new SpritePos(randX, randY);

		int boardSize = horizontalPosCount * verticalPosCount;
		if(snakeBody.size() >= boardSize){
			System.out.println("you won!");
			applePos = null;
			return; //you won
		} else if(snakeBody.contains(candidatePos)){
			placeRandomApple();
		}
		else {
			applePos = new SpritePos(randX, randY);
		}
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
		snakeDirectionQueue.push(SnakeDirection.RIGHT);
		placeRandomApple();
		
		if(gameLoopTimer==null){
			gameLoopTimer = new Timer(speed, e -> {
				if(GameState.RUNNING.equals(gameState)){
					gameState = GameState.RUNNING;
					moveSnake();
					repaint();
				}
			});
		} else {
			gameLoopTimer.setDelay(speed);
		}
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		doDrawing(g);
	}

	private void doDrawing(Graphics g) {
		
		Font font = new JLabel().getFont();
		FontMetrics metr = getFontMetrics(font);
		g.setFont(font);

		int screenWidth = getWidth();
		int screenHeight = getHeight();

		if(GameState.NOT_STARTED.equals(gameState)){
			//Draw pause screen
			String title = "Octane Snake";
			g.drawString(title, (screenWidth- metr.stringWidth(title)) / 2, screenHeight / 2);
			String click = "Click to start, space to pause";
			g.drawString(click, (screenWidth - metr.stringWidth(click)) / 2 , screenHeight / 2 + 20);
		} else if(GameState.PAUSED.equals(gameState)){
			//Draw pause screen
			g.drawString("Paused", (screenWidth- metr.stringWidth("Paused")) / 2, screenHeight / 2 - 20);
			String score = "Score: " + (snakeBody.size() - INIT_SIZE);
			g.drawString(score, (screenWidth - metr.stringWidth(score)) / 2 , screenHeight / 2);
			String click = "Press space to resume";
			g.drawString(click, (screenWidth - metr.stringWidth(click)) / 2 , screenHeight / 2 + 20);

		} else if(GameState.RUNNING.equals(gameState)){
			//Draw the snake according to the game state

			int spriteWidth = screenWidth / horizontalPosCount;
			int spriteHeight = screenHeight / verticalPosCount;

			for(int x = 0; x<horizontalPosCount;x++){
				for(int y = 0; y<verticalPosCount;y++){
					g.drawChars((x+""+y).toCharArray(), 0, 2, spriteWidth * x + (spriteWidth/2) , spriteHeight * y + (spriteHeight/2) );
				}	
			}

			//draw apple
			if(applePos != null){
				g.drawImage(APPLE_SPRITE, spriteWidth * applePos.x, spriteHeight * applePos.y, spriteWidth, spriteHeight, this);
			}

			//draw the snake at it's current position
			for(SpritePos pos : snakeBody){
				if(snakeBody.indexOf(pos) == 0){
					g.drawImage(HEAD_SPRITE, spriteWidth * pos.x, spriteHeight * pos.y, spriteWidth, spriteHeight, this);
				} else {
					g.drawImage(BODY_SPRITE, spriteWidth * pos.x, spriteHeight * pos.y, spriteWidth, spriteHeight, this);
				}

			}
		} else if(GameState.OVER.equals(gameState)){
			g.drawString("Game Over", (screenWidth- metr.stringWidth("Game Over")) / 2, screenHeight / 2 - 20);
			String score = "Score: " + (snakeBody.size() - INIT_SIZE);
			g.drawString(score, (screenWidth - metr.stringWidth(score)) / 2 , screenHeight / 2);
			String click = "Click to restart";
			g.drawString(click, (screenWidth - metr.stringWidth(click)) / 2 , screenHeight / 2 + 20);
		}
		
		Toolkit.getDefaultToolkit().sync();


	}

	private String printSnake(){
		String snake = snakeBody
				.stream()
				.map(pos -> "[x: " + pos.x + ", y: " + pos.y+"]")
				.collect(Collectors.joining(", "));
		return snake;
	}

	private void setKeyBindings() {
		ActionMap actionMap = getActionMap();
		int condition = JComponent.WHEN_IN_FOCUSED_WINDOW;
		InputMap inputMap = getInputMap(condition );

		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0), SnakeDirection.LEFT);
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0), SnakeDirection.RIGHT);
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0), SnakeDirection.UP);
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0), SnakeDirection.DOWN);
		inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, 0), "SPACE");

		actionMap.put(SnakeDirection.LEFT, new KeyAction(SnakeDirection.LEFT, SnakeDirection.RIGHT));
		actionMap.put(SnakeDirection.RIGHT, new KeyAction(SnakeDirection.RIGHT,SnakeDirection.LEFT));
		actionMap.put(SnakeDirection.UP, new KeyAction(SnakeDirection.UP, SnakeDirection.DOWN));
		actionMap.put(SnakeDirection.DOWN, new KeyAction(SnakeDirection.DOWN, SnakeDirection.UP));
		actionMap.put("SPACE", new PauseUnPauseAction());
	}
	
	private class PauseUnPauseAction extends AbstractAction {

		private static final long serialVersionUID = 1L;

		@Override
		public void actionPerformed(ActionEvent e) {
			if(GameState.PAUSED.equals(gameState)){
				gameState = GameState.RUNNING;
			} else if (GameState.RUNNING.equals(gameState)){
				gameState = GameState.PAUSED;
			}
			repaint();
		}
	}

	private class KeyAction extends AbstractAction {

		private static final long serialVersionUID = 1L;
		SnakeDirection direction;
		SnakeDirection oppositeDir;

		public KeyAction(SnakeDirection direction, SnakeDirection oppositeDir) {
			putValue(ACTION_COMMAND_KEY, direction);
			this.direction = direction;
			this.oppositeDir = oppositeDir;
		}

		@Override
		public void actionPerformed(ActionEvent actionEvt) {
			if(!snakeDirectionQueue.peekLast().equals(direction) && !snakeDirectionQueue.peekLast().equals(oppositeDir) && snakeDirectionQueue.size()<4){
				snakeDirectionQueue.add(direction);
			}
		}
	}

}
