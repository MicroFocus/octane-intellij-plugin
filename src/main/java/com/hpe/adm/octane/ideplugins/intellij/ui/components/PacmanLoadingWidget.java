package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import com.hpe.adm.octane.ideplugins.intellij.util.Constants;

import javax.swing.*;
import java.awt.*;

public class PacmanLoadingWidget extends JPanel {

    public PacmanLoadingWidget(){
        this(null);
    }

    public PacmanLoadingWidget(String loadingMessage){
        setLayout(new BorderLayout(0,0));
        //setBorder(IdeBorderFactory.createBorder());

        ImageIcon pacmanImage = new ImageIcon(PacmanLoadingWidget.class.getClassLoader().getResource(Constants.IMG_AJAX_SPINNER));
        JLabel loadingLabel = new JLabel(pacmanImage);
        loadingLabel.setText(loadingMessage);

        // loadingLabel.setIcon(IconLoader.findIcon(Constants.IMG_AJAX_SPINNER));
        // loadingLabel.setText(StringUtils.isEmpty(loadingMessage) ? "" : loadingMessage);
        add(loadingLabel, BorderLayout.CENTER);
    }

}
