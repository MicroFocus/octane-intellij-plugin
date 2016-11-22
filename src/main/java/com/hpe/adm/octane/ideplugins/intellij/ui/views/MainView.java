package com.hpe.adm.octane.ideplugins.intellij.ui.views;

import com.google.inject.Guice;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasContent;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import java.awt.*;

public class MainView extends HasContent {

    private final Border marginBorder = new EmptyBorder(5,5,5,5);
    private final JPanel rootPanel = new JPanel();

    //2 main components
    private TabView tabView = new TabView();

    public MainView(){
        Guice.createInjector(new PluginModule()).injectMembers(this); //TODO: DI issues
    }

    private boolean isInit = false;
    private void initView(){
        if(!isInit) {
            JPanel tabPanel = new JPanel(new BorderLayout(0, 0));
            tabPanel.setBorder(marginBorder);
            tabPanel.add(tabView.getComponent(), BorderLayout.CENTER);
            //add the tabPanel to the root panel
            GridLayoutManager gridLayoutManager = new GridLayoutManager(1, 1);
            rootPanel.setLayout(gridLayoutManager);
            GridConstraints gc = new GridConstraints();
            gc.setFill(GridConstraints.FILL_BOTH);
            rootPanel.add(tabPanel, gc);
            isInit = true;
        }
    }

    @Override
    public JComponent getComponent() {
        initView();
        return rootPanel;
    }

}
