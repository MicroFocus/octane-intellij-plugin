/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.html.JavaFxHtmlPanel;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.IdeBorderFactory;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.UIUtil;
import javafx.application.Platform;
import org.apache.commons.lang.StringUtils;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Collection;

public class CommentsPanel extends JPanel {

    private JButton sendMessageButton;
    private JTextField messageBox;

    private JavaFxHtmlPanel commentsPanel;
    private String commentContent = "";
    private ActionListener addCommentActionListener;

    @Inject
    public CommentsPanel(JavaFxHtmlPanel commentsPanel) {

        this.commentsPanel = commentsPanel;

        setLayout(new BorderLayout());

        JPanel messagePanel = new JPanel();
        messagePanel.setLayout(new GridBagLayout());

        messageBox = new JTextField(30);
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
                if (e.getKeyCode() == KeyEvent.VK_ENTER &&
                        addCommentActionListener != null &&
                        sendMessageButton.isEnabled()) {
                    addCommentActionListener.actionPerformed(null);
                }
            }
        });
        sendMessageButton = new JButton("Post");

        GridBagConstraints left = new GridBagConstraints();
        left.anchor = GridBagConstraints.LINE_START;
        left.fill = GridBagConstraints.HORIZONTAL;
        left.weightx = 512.0D;
        left.weighty = 1.0D;

        GridBagConstraints right = new GridBagConstraints();
        right.insets = JBUI.insetsLeft(10);
        right.anchor = GridBagConstraints.LINE_END;
        right.fill = GridBagConstraints.NONE;

        messagePanel.add(messageBox, left);
        messagePanel.add(sendMessageButton, right);
        messagePanel.setBorder(BorderFactory.createEmptyBorder(0, 0, 5 ,0));
        add(BorderLayout.NORTH, messagePanel);

        commentsPanel.setBorder(IdeBorderFactory.createBorder());
        add(commentsPanel, BorderLayout.CENTER);
        enableButton();
    }

    private void addExistingComment(String commentPostDate, String username, String message) {
        commentContent += commentPostDate + " <b>" + username + ":</b> <br>" + message + "<hr>";
    }

    private void setChatBoxScene() {
        Platform.runLater(() -> commentsPanel.setHtmlContent(commentContent));
    }


    public void addSendNewCommentAction(ActionListener actionListener) {
        this.addCommentActionListener = actionListener;
        removeAnyPreviousListener();
        sendMessageButton.addActionListener(actionListener);
    }

    private void removeAnyPreviousListener(){
        ActionListener[] listeners = sendMessageButton.getActionListeners();
        for (ActionListener listener : listeners) {
            sendMessageButton.removeActionListener(listener);
        }
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

    private void clearCurrentComments() {
        commentContent = "";
    }

    public void setComments(Collection<EntityModel> comments) {
        clearCurrentComments();
        for (EntityModel comment : comments) {
            String commentsPostTime = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_CREATION_TIME));
            String userName = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_AUTHOR), "full_name");
            String commentLine = Util.getUiDataFromModel(comment.getValue(DetailsViewDefaultFields.FIELD_COMMENT_TEXT));
            addExistingComment(commentsPostTime, userName, commentLine);
        }
        setChatBoxScene();
    }
}
