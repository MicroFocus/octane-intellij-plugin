package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.text.BadLocationException;

import com.intellij.ui.ClickListener;
import com.intellij.ui.JBColor;

public class CommentsConversationPanel extends JPanel {
	private JButton sendMessage;
	private JTextField messageBox;
	private JTextArea chatBox;
	private String username = "dasdas";

	public CommentsConversationPanel() {
		setLayout(new BorderLayout());

		JPanel southPanel = new JPanel();
		southPanel.setBackground(JBColor.border());
		southPanel.setLayout(new GridBagLayout());

		messageBox = new JTextField(30);
		messageBox.setBackground(JBColor.background());
		messageBox.setFont(new Font("Tahoma", Font.PLAIN, 11));
		messageBox.setOpaque(false);
		messageBox.requestFocusInWindow();

		sendMessage = new JButton("Add");
		sendMessage.addActionListener(new sendMessageButtonListener());

		chatBox = new JTextArea();
		chatBox.setBackground(JBColor.background());
		chatBox.setLineWrap(true);
		chatBox.setWrapStyleWord(true);
		chatBox.setEditable(false);
		chatBox.setOpaque(false);
		chatBox.setBorder(null);
		chatBox.setFont(new Font("Tahoma", Font.PLAIN, 11));

		GridBagConstraints left = new GridBagConstraints();
		left.anchor = GridBagConstraints.LINE_START;
		left.fill = GridBagConstraints.HORIZONTAL;
		left.weightx = 512.0D;
		left.weighty = 1.0D;

		GridBagConstraints right = new GridBagConstraints();
		right.insets = new Insets(0, 10, 0, 0);
		right.anchor = GridBagConstraints.LINE_END;
		right.fill = GridBagConstraints.NONE;
		right.weightx = 1.0D;
		right.weighty = 1.0D;

		southPanel.add(messageBox, left);
		southPanel.add(sendMessage, right);

		add(BorderLayout.NORTH, southPanel);
		JScrollPane scrollChatBox =  new JScrollPane(chatBox);
		scrollChatBox.setBorder(null);
		add(scrollChatBox, BorderLayout.CENTER);

	}
	
	public void addNewComment(){
		try {
			chatBox.getDocument().insertString(0,new Date().toString()+" "+username + "| "+ messageBox.getText() +"\n", null);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
	public void addExistingComment(String commentPostDate,String username,String message){
		try {
			chatBox.getDocument().insertString(0,"\n" + commentPostDate+" "+username + ": "+ "\n"+message +"\n", null);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

	class sendMessageButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent event) {
			if (messageBox.getText().length() < 1) {
				// do nothing
			} else {
				addNewComment();
				messageBox.setText("");
			}
			messageBox.requestFocusInWindow();
		}
	}
	public void addSendNewCommentAction(ActionListener actionListener) {
		sendMessage.addActionListener(actionListener);
	}

	public void setCommentMessageBoxText(String t) {
		messageBox.setText(t);
	}

	public String getCommentMessageBoxText() {
		return messageBox.getText();
	}
	public void clearCurrentComments(){
		chatBox.setText("");
	}
	public void scrollCommentListToTop(){
		chatBox.setCaretPosition(0);
	}
}
