package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.hpe.adm.octane.ideplugins.intellij.ui.custom.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.custom.HasContent;
import com.hpe.adm.octane.ideplugins.intellij.ui.custom.components.TransparentTree;

import javax.swing.*;

public class MainView extends HasContent {

    private JPanel rootPanel;
    private JTabbedPane tabbedPane;
    private JPanel filterPanel;
    private TransparentTree filterTree;

    public JPanel getRootPanel(){
        return rootPanel;
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

}
