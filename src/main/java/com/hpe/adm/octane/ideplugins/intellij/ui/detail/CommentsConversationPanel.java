/*
 * © 2018 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.util.ui.UIUtil;
import javafx.application.Platform;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class CommentsConversationPanel extends JPanel {
    private JButton sendMessageButton;
    private JTextField messageBox;
    private HTMLPresenterFXPanel chatBox;
    private String commentContent = "";
    private ActionListener addCommentActionListener;

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
        messageBox.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if(e.getKeyCode() == KeyEvent.VK_ENTER &&
                        addCommentActionListener != null &&
                        sendMessageButton.isEnabled()){
                    addCommentActionListener.actionPerformed(null);
                }
            }
        });
        sendMessageButton = new JButton("Add");



        chatBox = new HTMLPresenterFXPanel();
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
        add(chatBox, BorderLayout.CENTER);
        enableButton();
    }

    void addExistingComment(String commentPostDate, String username, String message) {
        commentContent += commentPostDate + " <b>" + username + ":</b> <br>" + message + "<hr>";
    }

    void setChatBoxScene() {
        Platform.runLater(() -> chatBox.setContent(commentContent));
        Platform.runLater(() -> chatBox.initFX());
    }


    public void addSendNewCommentAction(ActionListener actionListener) {
        this.addCommentActionListener = actionListener;
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
