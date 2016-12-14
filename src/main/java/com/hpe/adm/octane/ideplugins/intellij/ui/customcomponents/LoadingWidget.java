package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.hpe.adm.octane.ideplugins.intellij.util.Constants;

import javax.swing.*;
import java.awt.*;

public class LoadingWidget extends JPanel {

    public LoadingWidget() {
        this(null);
    }

    public LoadingWidget(String loadingMessage) {
        setLayout(new BorderLayout(0,0));
        ImageIcon pacmanImage = new ImageIcon(LoadingWidget.class.getClassLoader().getResource(Constants.IMG_AJAX_SPINNER));
        JLabel loadingLabel = new JLabel(pacmanImage);
        loadingLabel.setText(loadingMessage);
        add(loadingLabel, BorderLayout.CENTER);
    }

}
