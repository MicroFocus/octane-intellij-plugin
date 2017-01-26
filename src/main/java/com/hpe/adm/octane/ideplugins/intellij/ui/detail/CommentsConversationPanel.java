package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.HyperlinkEvent;
import java.awt.*;
import java.awt.event.ActionListener;

public class CommentsConversationPanel extends JPanel {
	private JButton sendMessageButton;
	private JTextField messageBox;
	private JTextPane chatBox;

	public CommentsConversationPanel() {
		setLayout(new BorderLayout());

		JPanel southPanel = new JPanel();
		southPanel.setLayout(new GridBagLayout());

		messageBox = new JTextField(30);
		messageBox.setFont(new Font("Tahoma", Font.PLAIN, 11));
		messageBox.setOpaque(false);
		messageBox.requestFocusInWindow();
		messageBox.setOpaque(true);
		messageBox.setBackground(UIUtil.getTextFieldBackground());

		sendMessageButton = new JButton("Add");

		chatBox = new JTextPane();
        chatBox.setContentType("text/html");
		chatBox.setEditable(false);
		chatBox.setOpaque(false);
		chatBox.setBorder(null);
		chatBox.setFont(new Font("Tahoma", Font.PLAIN, 11));
        chatBox.addHyperlinkListener(e -> {
            if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                try {
                    Desktop.getDesktop().browse(e.getURL().toURI());
                } catch (Exception e1) {}
            }
        });

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
        String currentText = removeHtmlStructure(chatBox.getText());
	    String strippedMessage = removeHtmlStructure(message);
	    //remove links, as they wont work in swing sadly
        //strippedMessage = strippedMessage.replaceAll("<a.*?>", "").replaceAll("</a>", "");
        currentText +=  commentPostDate + " <b>" + username + ":</b> <br>" + strippedMessage + "<hr>";
        chatBox.setText(currentText);
	}

	private String removeHtmlStructure(String htmlString){
        htmlString = htmlString.replace("<html>", "");
        htmlString = htmlString.replace("</html>", "");
        htmlString = htmlString.replace("<body>", "");
        htmlString = htmlString.replace("</body>", "");
        htmlString = htmlString.replace("<head>", "");
        htmlString = htmlString.replace("</head>", "");
        return htmlString;
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
