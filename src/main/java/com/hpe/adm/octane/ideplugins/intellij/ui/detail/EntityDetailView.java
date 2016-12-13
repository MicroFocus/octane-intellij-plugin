package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import java.awt.*;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;

public class EntityDetailView implements View {

    private JPanel entityDetailsPanel;

    public EntityDetailView() {

    }

    public static String getNameForEntity(Entity entity) {
        String ret = "Item";
        switch (entity) {
            case DEFECT:
                ret = "Defect";
                break;
            case GHERKIN_TEST:
                ret = "Gherkin Test";
                break;
            case MANUAL_TEST:
                ret = "Manual Test";
                break;
            case USER_STORY:
                ret = "User Story";
                break;
            case TASK:
                ret = "Task";
                break;
        }
        return ret;
    }

    @Override
    public JComponent getComponent() {
        JBScrollPane component = new JBScrollPane(entityDetailsPanel);
        component.setBorder(null);
        component.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_ALWAYS);
        component.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        component.setMinimumSize(new Dimension(0, 0));
        return component;
    }

    //TODO: @osavencu: build it more generic (after it works)
    public void setEntityModel(EntityModel entityModel) {
        entityDetailsPanel = new GeneralEntityDetailsPanel(entityModel);
    }


}
