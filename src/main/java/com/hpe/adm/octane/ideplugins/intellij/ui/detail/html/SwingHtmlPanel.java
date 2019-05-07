package com.hpe.adm.octane.ideplugins.intellij.ui.detail.html;

import javax.swing.*;
import java.awt.*;

public class SwingHtmlPanel extends HtmlPanel {

    private JEditorPane editorPane;

    public SwingHtmlPanel() {
        setLayout(new BorderLayout());
        this.editorPane = new JEditorPane();
        add(editorPane, BorderLayout.CENTER);
    }

    @Override
    public void setHtmlContent(String htmlContent) {
        editorPane.setContentType(htmlContent);
    }

    @Override
    public String getHtmlContent() {
        return editorPane.getText();
    }

}