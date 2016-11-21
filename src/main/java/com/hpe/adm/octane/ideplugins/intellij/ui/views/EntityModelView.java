package com.hpe.adm.octane.ideplugins.intellij.ui.views;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;

import javax.swing.*;
import java.awt.*;

public class EntityModelView {
    private JPanel entityModelView;
    private JScrollPane scrollableView;

    public EntityModelView() {

    }

    public EntityModelView(EntityModel entityModel) {
        entityModelView = new JPanel();
        entityModelView.setLayout(new GridLayout(0, 1));

        scrollableView = new JScrollPane(entityModelView);

        for (FieldModel fieldModel : entityModel.getValues()) {
            entityModelView.add(new JLabel(fieldModel.getName() + " : " + fieldModel.getValue()));
        }
    }

    public JPanel getEntityModelView() {
        return entityModelView;
    }

    public JScrollPane getEntityModelScrollableView() {
        return scrollableView;
    }
}
