package com.hpe.adm.octane.ideplugins.intellij.ui.views;

import com.google.inject.Guice;
import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.ClosableJTabbedPane;
import com.hpe.adm.octane.ideplugins.intellij.ui.views.treetable.EntityTreeTableView;

import javax.swing.*;
import java.awt.*;

public class TabView implements HasComponent {

    private final JPanel rootPanel = new JPanel();
    private ClosableJTabbedPane tabbedPane = new ClosableJTabbedPane();

    //TODO: @Inject a factory
    @Inject
    private EntityTreeTableView entityTreeTableView;

    public TabView(){
        Guice.createInjector(new PluginModule()).injectMembers(this); //TODO: DI issues

        rootPanel.setLayout(new BorderLayout(0, 0));
        rootPanel.add(tabbedPane, BorderLayout.CENTER);

        for(int i = 0; i<1; i++){
            tabbedPane.addTab("Tab " + i,  makeDummyTab());
        }
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

    private JPanel makeDummyTab(){
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout(0, 0));
        panel.add(entityTreeTableView.getComponent(), BorderLayout.CENTER);
        return panel;
    }

}
