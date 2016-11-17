package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.hpe.adm.octane.ideplugins.intellij.ui.custom.FilteringWidget;
import com.hpe.adm.octane.ideplugins.intellij.ui.custom.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.custom.HasContent;
import com.hpe.adm.octane.ideplugins.intellij.ui.custom.components.TransparentTree;

import javax.swing.*;
import java.awt.*;

public class MainView extends HasContent {

    private JPanel rootPanel;
    private JTabbedPane tabbedPane;
    private JPanel filterPanel;


    public JPanel getRootPanel(){
        return rootPanel;
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

    private void createUIComponents() {
        FilteringWidget filteringWidget = new FilteringWidget();
        filterPanel = new JPanel();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        filterPanel.add(filteringWidget.getComponent(), gbc);
    }
}
