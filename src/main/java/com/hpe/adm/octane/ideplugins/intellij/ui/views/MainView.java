package com.hpe.adm.octane.ideplugins.intellij.ui.views;

import com.google.inject.Guice;
import com.google.inject.Inject;
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
    @Inject
    private FilteringView filteringView;
    @Inject
    private TabView tabView;

    public MainView(){
        Guice.createInjector(new PluginModule()).injectMembers(this); //TODO: DI issues

        //Init view
        JSplitPane splitter = new JSplitPane();

        JPanel filterPanel = new JPanel(new BorderLayout(0,0));
        filterPanel.setBorder(marginBorder);
        filterPanel.add(filteringView.getComponent(), BorderLayout.CENTER);

        JPanel tabPanel = new JPanel(new BorderLayout(0,0));
        tabPanel.setBorder(marginBorder);
        tabPanel.add(tabView.getComponent(), BorderLayout.CENTER);

        //Add them to the splitter
        splitter.setLeftComponent(filteringView.getComponent());
        splitter.setRightComponent(tabPanel);

        //add the splitter to the root panel;
        GridLayoutManager gridLayoutManager = new GridLayoutManager(1,1);
        rootPanel.setLayout(gridLayoutManager);
        GridConstraints gc = new GridConstraints();
        gc.setFill(GridConstraints.FILL_BOTH);
        rootPanel.add(splitter, gc);
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

}
