package com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.snake;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

/**
 * Allows you to try out the snake game hidden in the IDE plugin
 */
class SnakeTestFrame extends JFrame {

	private static final long serialVersionUID = 1L;
	private JPanel contentPane;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(() -> {
            try {
                SnakeTestFrame frame = new SnakeTestFrame();
                frame.setVisible(true);

                JPanel panel = new JPanel(new BorderLayout(0,0));
                panel.setPreferredSize(new Dimension(800, 600));
                panel.add(new SnakeGame(), BorderLayout.CENTER);
                frame.getContentPane().add(panel);

                //Display the window.
                frame.pack();
                frame.setVisible(true);
                frame.setLocationRelativeTo(null);
                frame.repaint();
                frame.revalidate();

            } catch (Exception e) {
                e.printStackTrace();
            }
        });
	}

	/**
	 * Create the frame.
	 */
	public SnakeTestFrame() {
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 450, 300);
		contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		contentPane.setLayout(new BorderLayout(0, 0));
		setContentPane(contentPane);
	}

}