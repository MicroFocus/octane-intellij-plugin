package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.util.ui.UIUtil;
import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionListener;

public class CommentsConversationPanel extends JPanel {
    private JButton sendMessageButton;
    private JTextField messageBox;
    private CommentsFXPanel chatBox;
    private String commentContent = "";

    public CommentsConversationPanel() {
        setLayout(new BorderLayout());

        JPanel southPanel = new JPanel();
        southPanel.setLayout(new GridBagLayout());

        messageBox = new JTextField(30);
        messageBox.setFont(new Font("Arial", Font.PLAIN, 11));
        messageBox.setOpaque(false);
        messageBox.requestFocusInWindow();
        messageBox.setOpaque(true);
        messageBox.setBackground(UIUtil.getTextFieldBackground());
        messageBox.getDocument().addDocumentListener(new DocumentListener() {
            public void changedUpdate(DocumentEvent e) {
                enableButton();
            }

            public void removeUpdate(DocumentEvent e) {
                enableButton();
            }

            public void insertUpdate(DocumentEvent e) {
                enableButton();
            }
        });
        sendMessageButton = new JButton("Add");

        chatBox = new CommentsFXPanel();
        chatBox.setOpaque(false);
        chatBox.setBorder(null);
        chatBox.setFont(new Font("Arial", Font.PLAIN, 11));
        chatBox.addEventActions();

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
        southPanel.setBorder(new EmptyBorder(0, 5, 0, 5));

        add(BorderLayout.NORTH, southPanel);
        JScrollPane scrollChatBox = new JScrollPane(chatBox);
        scrollChatBox.setBorder(new EmptyBorder(0, 5, 0, 5));
        add(scrollChatBox, BorderLayout.CENTER);
        enableButton();
    }

    public void addExistingComment(String commentPostDate, String username, String message) {
        String strippedMessage = removeHtmlStructure(message);
        commentContent += commentPostDate + " <b>" + username + ":</b> <br>" + strippedMessage + "<hr>";
    }

    void setChatBoxScene() {
        Platform.setImplicitExit(false);

        Platform.runLater(() -> { // FX components need to be managed by JavaFX
            WebView webView = new WebView();
            WebEngine webEngine = webView.getEngine();

            webEngine.loadContent(commentContent);
            chatBox.setWebView(webView);
            chatBox.setScene(new Scene(webView));
        });

        Platform.runLater(() -> chatBox.initFX());
    }

    private String removeHtmlStructure(String htmlString) {
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

    private void enableButton() {
        if (StringUtils.isBlank(messageBox.getText())) {
            sendMessageButton.setEnabled(false);
        } else {
            sendMessageButton.setEnabled(true);
        }
    }

    public void setCommentMessageBoxText(String t) {
        messageBox.setText(t);
    }

    public String getCommentMessageBoxText() {
        return messageBox.getText();
    }

    public void clearCurrentComments() {
        commentContent = "";
    }
}
