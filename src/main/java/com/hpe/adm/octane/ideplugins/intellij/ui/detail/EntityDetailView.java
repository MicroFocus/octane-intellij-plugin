package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PacmanLoadingWidget;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import java.awt.*;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;

public class EntityDetailView implements View {

    private JBScrollPane component = new JBScrollPane(new PacmanLoadingWidget("Loading ...."));
    private JPanel entityDetailsPanel;

    public EntityDetailView() {

    }
    @Override
    public JComponent getComponent() {
        component.setBorder(null);
        component.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_ALWAYS);
        component.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        component.setMinimumSize(new Dimension(0, 0));
        return component;
    }

    public void setEntityModel(EntityModel entityModel) {
        entityDetailsPanel = new GeneralEntityDetailsPanel(entityModel);
        component.setViewportView(entityDetailsPanel);
    }

}
