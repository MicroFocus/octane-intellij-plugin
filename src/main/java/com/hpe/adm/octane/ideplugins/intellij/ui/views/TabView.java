package com.hpe.adm.octane.ideplugins.intellij.ui.views;

import com.google.inject.Guice;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.ClosableJTabbedPane;
import com.hpe.adm.octane.ideplugins.intellij.ui.views.treetable.EntityTreeTableView;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import javax.swing.*;
import java.awt.*;

public class TabView implements HasComponent {

    private final JPanel rootPanel = new JPanel();
    private ClosableJTabbedPane tabbedPane = new ClosableJTabbedPane();

    private TestService testService = PluginModule.getInstance(TestService.class);

    public TabView(){
        Guice.createInjector(new PluginModule()).injectMembers(this); //TODO: DI issues

        rootPanel.setLayout(new BorderLayout(0, 0));
        rootPanel.add(tabbedPane, BorderLayout.CENTER);
    }

    @Override
    public JComponent getComponent() {
        tabbedPane.addTab("Tab ", makeTab());
        tabbedPane.addTab("Detailed ", makeDetailedTab());
        return rootPanel;
    }

    private JPanel makeTab() {
        EntityTreeTableView entityTreeTableView = new EntityTreeTableView();
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout(0, 0));
        panel.add(entityTreeTableView.getComponent(), BorderLayout.CENTER);
        return panel;
    }

    private JScrollPane makeDetailedTab() {
        EntityModel entityModel = testService.findEntities(Entity.WORK_ITEM).iterator().next();
        EntityModelView entityModelView = new EntityModelView(entityModel);
        return entityModelView.getEntityModelScrollableView();
    }

}
