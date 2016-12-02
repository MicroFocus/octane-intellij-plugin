package com.hpe.adm.octane.ideplugins.intellij.ui.entityicon;

import java.awt.*;

class IconDetail{

    private Color color;
    private String displayLabelText;
    private boolean isOpaque = true;

    public IconDetail(Color color, String displayLabelText) {
        this.color = color;
        this.displayLabelText = displayLabelText;
    }

    public IconDetail(Color color, String displayLabelText, boolean isOpaque) {
        this.color = color;
        this.displayLabelText = displayLabelText;
        this.isOpaque = isOpaque;
    }

    public Color getColor() {
        return color;
    }

    public String getDisplayLabelText() {
        return displayLabelText;
    }

    public boolean isOpaque() {
        return isOpaque;
    }
}
