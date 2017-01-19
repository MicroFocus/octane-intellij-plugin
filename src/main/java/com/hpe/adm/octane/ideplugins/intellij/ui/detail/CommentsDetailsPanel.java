package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.BoxLayout;

import org.jdesktop.swingx.JXButton;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;

import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.OneCommetFatLine;
import javax.swing.JScrollPane;
import org.jdesktop.swingx.JXLabel;
import java.awt.BorderLayout;

public class CommentsDetailsPanel extends JXPanel {
	private JXPanel commentListPanel;
	private GridBagConstraints gbc_oneColumn;

	public CommentsDetailsPanel() {
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{0, 0};
		gridBagLayout.rowHeights = new int[]{0, 0, 0};
		gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBorder(null);
		GridBagConstraints gbc_scrollPane = new GridBagConstraints();
		gbc_scrollPane.fill = GridBagConstraints.BOTH;
		gbc_scrollPane.gridx = 0;
		gbc_scrollPane.gridy = 1;
		add(scrollPane, gbc_scrollPane);
		
		commentListPanel = new JXPanel();
		scrollPane.setViewportView(commentListPanel);
		commentListPanel.setLayout(new BorderLayout(0, 0));
		
		gbc_oneColumn = new GridBagConstraints();
		gbc_oneColumn.gridx = 0;
		gbc_oneColumn.gridy = 0;
		
		JXPanel addCommentsPanel = new JXPanel();
		GridBagConstraints gbc_addCommentsPanel = new GridBagConstraints();
		gbc_addCommentsPanel.insets = new Insets(0, 0, 5, 0);
		gbc_addCommentsPanel.fill = GridBagConstraints.BOTH;
		gbc_addCommentsPanel.gridx = 0;
		gbc_addCommentsPanel.gridy = 0;
		add(addCommentsPanel, gbc_addCommentsPanel);
		GridBagLayout gbl_addCommentsPanel = new GridBagLayout();
		gbl_addCommentsPanel.columnWidths = new int[]{0, 0, 0};
		gbl_addCommentsPanel.rowHeights = new int[]{0, 0};
		gbl_addCommentsPanel.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		gbl_addCommentsPanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		addCommentsPanel.setLayout(gbl_addCommentsPanel);
		
		JXTextArea txtrAddComment = new JXTextArea();
		
		GridBagConstraints gbc_txtrAddComment = new GridBagConstraints();
		gbc_txtrAddComment.insets = new Insets(0, 0, 0, 5);
		gbc_txtrAddComment.fill = GridBagConstraints.BOTH;
		gbc_txtrAddComment.gridx = 0;
		gbc_txtrAddComment.gridy = 0;
		addCommentsPanel.add(txtrAddComment, gbc_txtrAddComment);
		
		JXButton btnSend = new JXButton();
		btnSend.addActionListener(new ActionListener() {          
		    public void actionPerformed(ActionEvent e) {
		         addNewComment("gigi", "now", txtrAddComment.getText());
		    }
		}); 
		btnSend.setText("Send");
		GridBagConstraints gbc_btnSend = new GridBagConstraints();
		gbc_btnSend.anchor = GridBagConstraints.SOUTH;
		gbc_btnSend.gridx = 1;
		gbc_btnSend.gridy = 0;
		addCommentsPanel.add(btnSend, gbc_btnSend);

	}
	public void addNewComment(String userName, String commetPostDate, String comment){
		commentListPanel.add(new OneCommetFatLine(userName,commetPostDate,comment));
//		commentListPanel.add(new OneCommetFatLine(userName,commetPostDate,comment), gbc_oneColumn,0);
	}

}