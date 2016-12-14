package com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

public class SnakeTestFrame extends JFrame {

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
                panel.add(new ResizableSnakeGame(), BorderLayout.CENTER);
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
