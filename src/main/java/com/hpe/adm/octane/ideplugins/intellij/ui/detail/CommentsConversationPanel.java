package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.text.BadLocationException;
import java.awt.*;
import java.awt.event.ActionListener;

public class CommentsConversationPanel extends JPanel {
	private JButton sendMessageButton;
	private JTextField messageBox;
	private JTextArea chatBox;
	private String username;

	public CommentsConversationPanel() {
		setLayout(new BorderLayout());

		JPanel southPanel = new JPanel();
		southPanel.setLayout(new GridBagLayout());

		messageBox = new JTextField(30);
		messageBox.setFont(new Font("Tahoma", Font.PLAIN, 11));
		messageBox.setOpaque(false);
		messageBox.requestFocusInWindow();

		sendMessageButton = new JButton("Add");

		chatBox = new JTextArea();
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
		southPanel.add(sendMessageButton, right);
        southPanel.setBorder(new EmptyBorder(0,5,0,5));

		add(BorderLayout.NORTH, southPanel);
		JScrollPane scrollChatBox =  new JScrollPane(chatBox);
		scrollChatBox.setBorder(new EmptyBorder(0,5,0,5));
		add(scrollChatBox, BorderLayout.CENTER);

	}
	


	public void addExistingComment(String commentPostDate, String username, String message){
		try {
			chatBox.getDocument().insertString(0,"\n" + commentPostDate+" "+username + ": "+ "\n"+message +"\n", null);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

	public void addSendNewCommentAction(ActionListener actionListener) {
		sendMessageButton.addActionListener(actionListener);
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
