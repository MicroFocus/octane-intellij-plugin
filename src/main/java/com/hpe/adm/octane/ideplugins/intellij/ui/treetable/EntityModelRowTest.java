package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import java.awt.BorderLayout;
import java.awt.EventQueue;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

public class EntityModelRowTest extends JFrame {

	private JPanel contentPane;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					EntityModelRowTest frame = new EntityModelRowTest();
					frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the frame.
	 */
	public EntityModelRowTest() {
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 450, 300);
		contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		contentPane.setLayout(new BorderLayout(0, 0));
		
		EntityModelRow row = new EntityModelRow();
		
		row.setIcon(Entity.DEFECT);
		
		row.setEntityName("UGBBGUABGUABGUABGUABGUABUGBAUGBAUGBAUGBAUBGUABGUAQBUABUGBAUGBAUBGUABGUABGUABGU");
		row.setEntityDetails("WEQKWKOEKOPQWKOPEWQKOP", "EMPTY");
		
		row.addDetailsTop("ASD: " + 213);
		row.addDetailsTop("FGH: " + 213);
		row.addDetailsTop("JKL: " + 213);
		
		row.addDetailsBottom("Invested hours: " + 213);
		row.addDetailsBottom("Invested hours: " + 213);
		row.addDetailsBottom("Invested hours: " + 213);
		
		contentPane.add(row);
		setContentPane(contentPane);
	}

}
