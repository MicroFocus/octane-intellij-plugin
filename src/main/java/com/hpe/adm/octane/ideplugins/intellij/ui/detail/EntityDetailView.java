package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;

import javax.swing.*;
import java.awt.*;

public class EntityDetailView implements View {

    private JPanel entityModelView;
    private EntityDetailPresenter presenter;

    public EntityDetailView(EntityDetailPresenter presenter) {
        this.presenter = presenter;

        entityModelView = new JPanel();
        entityModelView.setLayout(new GridLayout(0, 1));
    }

    public void setEntityModel(EntityModel entityModel){
        entityModelView.removeAll();

        for (FieldModel fieldModel : entityModel.getValues()) {
            entityModelView.add(new JLabel(fieldModel.getName() + " : " + fieldModel.getValue()));
        }
    }

    @Override
    public JComponent getComponent() {
        return new JScrollPane(entityModelView);
    }

    @Override
    public Presenter getPresenter() {
        return null;
    }

    @Override
    public void setPresenter(Presenter presenter) {

    }
}
