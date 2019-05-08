package com.hpe.adm.octane.ideplugins.intellij.ui.detail.html;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;

public class SwingHtmlPanel extends HtmlPanel {

    private JEditorPane editorPane;

    public SwingHtmlPanel() {
        setBorder(BorderFactory.createLineBorder(Color.WHITE));
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        editorPane = new JEditorPane();
        editorPane.setContentType("text/html");
        editorPane.setEditable(false);
        add(editorPane);
    }

    @Override
    public void setHtmlContent(String htmlContent) {
        editorPane.setText(htmlContent);
    }

    @Override
    public String getHtmlContent() {
        return editorPane.getText();
    }

}