package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.List;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

public class Test {

	public static void main(String[] args)
	{
		SwingUtilities.invokeLater(new Runnable(){
			public void run(){
				new Test().buildGUI();
			}
		});
	}

	public void buildGUI()
	{
		JPanel p = new JPanel(new BorderLayout());
		final JButton btn = new JButton("Do Some Long Task");
		p.add(btn,BorderLayout.SOUTH);
		final JFrame f = new JFrame();
		f.getContentPane().add(p);
		f.setSize(400,300);
		f.setLocationRelativeTo(null);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setVisible(true);
		btn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				final JDialog d = new JDialog();
				JPanel p1 = new JPanel(new GridBagLayout());
				p1.add(new JLabel("Please Wait..."),new GridBagConstraints());
				d.getContentPane().add(p1);
				d.setSize(100,100);
				d.setLocationRelativeTo(f);
				d.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
				d.setModal(true);

				Thread t = new Thread(){
					public void run(){
						for (int x = 0; x <= 100; x+=10)
						{
							final int selection = x;
							SwingUtilities.invokeLater(new Runnable(){//do swing work on EDT
								public void run(){
									btn.setText("long task up to "+selection+"%");
								}
							});
							try
							{
								Thread.sleep(1000);
							}
							catch (InterruptedException e) {e.printStackTrace();}
						}
						SwingUtilities.invokeLater(new Runnable(){//do swing work on EDT
							public void run(){
								d.dispose();
								btn.setText("Do Some Long Task");
							}
						});
					}
				};
				t.start();
				d.setVisible(true);
			}
		});
	}
}
