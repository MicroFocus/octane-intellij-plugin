package com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork;

import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.intellij.ui.JBColor;
import com.intellij.util.ImageLoader;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.concurrent.ThreadLocalRandom;

public class SnakeGame extends JPanel implements ActionListener {

    private int boardWidth = 0;
    private int boardHeight = 0;

    private int dotSize = 13;

    private final int ALL_DOTS = 900;

    private int INIT_DELAY = 120;
    private int delay = INIT_DELAY;

    private final int x[] = new int[ALL_DOTS];
    private final int y[] = new int[ALL_DOTS];

    private final int INIT_DOTS = 3;
    private int dots = INIT_DOTS;

    private int apple_x;
    private int apple_y;

    private boolean leftDirection = false;
    private boolean rightDirection = true;
    private boolean upDirection = false;
    private boolean downDirection = false;
    private boolean inGame = true;

    private Timer timer;
    private Image ball;
    private Image apple;
    private Image head;

    public SnakeGame(Dimension dimension) {

        boardWidth = (int) dimension.getWidth();

        if(boardWidth % dotSize != 0){
            boardWidth = boardWidth - (boardWidth % dotSize);
        }

        boardHeight = (int) dimension.getHeight();
        if(boardHeight % boardHeight != 0){
            boardHeight = boardHeight - (boardHeight % boardHeight);
        }

        if(boardWidth * 2 > boardHeight){
            boardWidth = boardHeight * 2;
        }
        if (boardHeight * 2 > boardWidth){
            boardHeight = boardWidth * 2;
        }


        addKeyListener(new TAdapter());
        setBackground(JBColor.background().darker());
        setFocusable(true);
        setBorder(BorderFactory.createLineBorder(JBColor.border()));

        setPreferredSize(new Dimension(boardWidth, boardHeight));
        loadImages();
        initGame();

        //Restart on click
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                requestFocus();
                if(!inGame) {
                    inGame = true;
                    initGame();
                }
            }
        });
    }

    private void loadImages() {

        ImageIcon iid = new ImageIcon(Constants.IMG_OCTANE_ICON);
        ball = ImageLoader.loadFromResource(Constants.IMG_OCTANE_ICON);

        ImageIcon iia = new ImageIcon(Constants.IMG_OCTANE_ICON_RED);
        apple = ImageLoader.loadFromResource(Constants.IMG_OCTANE_ICON_RED);

        ImageIcon iih = new ImageIcon(Constants.IMG_OCTANE_ICON_RED);
        head = ImageLoader.loadFromResource(Constants.IMG_OCTANE_ICON_RED);
    }

    private void initGame() {

        delay = INIT_DELAY;
        dots = INIT_DOTS;

        for (int z = 0; z < dots; z++) {
            x[z] = (boardWidth / 2) - (boardWidth / 2 % dotSize);
            y[z] = (boardHeight / 2) - (boardHeight / 2 % dotSize);
        }

        locateApple();

        if(timer==null){
            timer = new Timer(delay, this);
        } else {
            timer.setDelay(delay);
        }
        timer.start();
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        doDrawing(g);
    }

    private void doDrawing(Graphics g) {

        if (inGame) {

            g.drawImage(apple, apple_x, apple_y, this);

            for (int z = 0; z < dots; z++) {
                if (z == 0) {
                    g.drawImage(head, x[z], y[z], this);
                } else {
                    g.drawImage(ball, x[z], y[z], this);
                }
            }

            Toolkit.getDefaultToolkit().sync();

        } else {

            gameOver(g);
        }
    }

    private void gameOver(Graphics g) {

        String msg = "Game Over";
        Font small = new Font("Helvetica", Font.BOLD, 14);
        FontMetrics metr = getFontMetrics(small);
        g.setColor(UIUtil.getLabelBackground().brighter());
        g.setFont(small);
        g.drawString(msg, (boardWidth - metr.stringWidth(msg)) / 2, boardHeight / 2 - 20);

        String score = "Score: " + (dots - INIT_DOTS);
        g.drawString(score, (boardWidth - metr.stringWidth(score)) / 2 , boardHeight / 2);

        String click = "Click to restart";
        g.drawString(click, (boardWidth - metr.stringWidth(click)) / 2 , boardHeight / 2 + 20);

        timer.stop();
    }

    private void checkApple() {

        if ((x[0] == apple_x) && (y[0] == apple_y)) {

            dots++;
            delay -= 5;
            timer.setDelay(delay);
            locateApple();
        }
    }

    private void move() {

        for (int z = dots; z > 0; z--) {
            x[z] = x[(z - 1)];
            y[z] = y[(z - 1)];
        }

        if (leftDirection) {
            x[0] -= dotSize;
        }

        if (rightDirection) {
            x[0] += dotSize;
        }

        if (upDirection) {
            y[0] -= dotSize;
        }

        if (downDirection) {
            y[0] += dotSize;
        }
    }

    private void checkCollision() {

        for (int z = dots; z > 0; z--) {

            if ((z > 4) && (x[0] == x[z]) && (y[0] == y[z])) {
                inGame = false;
            }
        }

        if (y[0] >= boardHeight - dotSize) {
            inGame = false;
        }

        if (y[0] < 0) {
            inGame = false;
        }

        if (x[0] >= boardWidth) {
            inGame = false;
        }

        if (x[0] < 0) {
            inGame = false;
        }

        if(!inGame) {
            timer.stop();
        }
    }

    private void locateApple() {

        int r = ThreadLocalRandom.current().nextInt(0, boardWidth/dotSize);
        apple_x = ((r * dotSize));

        r = ThreadLocalRandom.current().nextInt(0, boardHeight/dotSize);
        apple_y = ((r * dotSize));
    }

    @Override
    public void actionPerformed(ActionEvent e) {

        if (inGame) {
            checkApple();
            checkCollision();
            move();
        }

        repaint();
    }

    private class TAdapter extends KeyAdapter {

        @Override
        public void keyPressed(KeyEvent e) {

            int key = e.getKeyCode();

            if ((key == KeyEvent.VK_LEFT) && (!rightDirection)) {
                leftDirection = true;
                upDirection = false;
                downDirection = false;
            }

            if ((key == KeyEvent.VK_RIGHT) && (!leftDirection)) {
                rightDirection = true;
                upDirection = false;
                downDirection = false;
            }

            if ((key == KeyEvent.VK_UP) && (!downDirection)) {
                upDirection = true;
                rightDirection = false;
                leftDirection = false;
            }

            if ((key == KeyEvent.VK_DOWN) && (!upDirection)) {
                downDirection = true;
                rightDirection = false;
                leftDirection = false;
            }
        }
    }

    public void pause(){
        timer.stop();
    }

    public void start(){
        timer.start();
    }
}