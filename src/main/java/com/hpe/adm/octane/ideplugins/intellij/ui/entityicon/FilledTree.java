package com.hpe.adm.octane.ideplugins.intellij.ui.entityicon;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.SwingConstants;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.AbstractLayoutCache;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeCellRenderer;

public class FilledTree {
	

	DefaultMutableTreeNode heading = new DefaultMutableTreeNode(" SHOW ALL");
	JTree database;
	JFrame frame;

	public FilledTree(){
		for(int i=0;i<10;i++){
			DefaultMutableTreeNode temp = new DefaultMutableTreeNode("No:"+i);
			heading.add(temp);
		}

		database = new JTree(heading);
		database.setCellRenderer(new TreeRenderer());
		database.setUI(new CustomTreeUI());
		database.setLargeModel(true);
		database.setRowHeight(25);

		frame = new JFrame();
		frame.add(database);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}

	public static void main(String[]args){
		new FilledTree();
	}

	private class TreeRenderer implements TreeCellRenderer{

		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean selected, boolean expanded, boolean leaf, int row,
				boolean hasFocus){
			
			JPanel panel = new JPanel();
			panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));

			JLabel label = null;;
			try {
				label = new TextOverImageLabel(new ImageIcon( new URL("http://forums.sun.com/im/bronze-star.gif"))," "+value.toString());
				
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			return panel;
		}
	}

	private class TextOverImageLabel extends JLabel {

		TextOverImageLabel(ImageIcon icon, String text) {
			super(text, icon, SwingConstants.LEFT); // ignored
			this.setFont(new Font("Lucida Grande",Font.BOLD,12));
			this.setBackground(Color.BLACK);
		}

		public void paintComponent(Graphics g) {
			g.drawImage(((ImageIcon)getIcon()).getImage(),0,0,getWidth(),getHeight(),null);
			g.drawString(getText(), 1, 16);
		}
	}

	public class CustomTreeUI extends BasicTreeUI {

		@Override
		protected AbstractLayoutCache.NodeDimensions createNodeDimensions() {
			return new NodeDimensionsHandler() {
				@Override
				public Rectangle getNodeDimensions(
						Object value, int row, int depth, boolean expanded,
						Rectangle size) {
					Rectangle dimensions = super.getNodeDimensions(value, row,
							depth, expanded, size);
					dimensions.width = database.getWidth() - getRowX(row, depth);
					return dimensions;
				}
			};
		}
	}
}
