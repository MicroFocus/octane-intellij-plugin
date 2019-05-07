package com.hpe.adm.octane.ideplugins.intellij.ui.detail.html;

import javax.swing.*;

/**
 * Not an interface because swing is ancient
 */
public abstract class HtmlPanel extends JPanel {

    public abstract void setHtmlContent(String htmlContent);
    public abstract String getHtmlContent();

}