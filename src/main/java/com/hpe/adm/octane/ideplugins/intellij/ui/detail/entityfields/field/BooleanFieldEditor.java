package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.BooleanFieldModel;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.openapi.ui.ComboBox;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class BooleanFieldEditor extends JPanel implements FieldEditor {
    protected EntityModelWrapper entityModelWrapper;
    protected String fieldName;

    private static String FIELD_NAME = "name";

    private ComboBox<EntityModel> booleanEntityComboBox;

    private static final EntityModel ENTITY_TRUE = new EntityModel();
    private static final EntityModel ENTITY_FALSE = new EntityModel();
    static {
        ENTITY_TRUE.setValue(new StringFieldModel(FIELD_NAME, Boolean.TRUE.toString()));
        ENTITY_FALSE.setValue(new StringFieldModel(FIELD_NAME, Boolean.FALSE.toString()));
    }

    public BooleanFieldEditor() {
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[] { 0, 0};
        layout.columnWeights = new double[] { 0.0, 0.0};
        setLayout(layout);

        booleanEntityComboBox = new ComboBox();
        booleanEntityComboBox.addItem(ENTITY_TRUE);
        booleanEntityComboBox.addItem(ENTITY_FALSE);

        booleanEntityComboBox.setCursor(new Cursor(Cursor.HAND_CURSOR));

        booleanEntityComboBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                boolean newValue = booleanEntityComboBox.getSelectedItem() == ENTITY_TRUE ? true : false;
                entityModelWrapper.setValue(new BooleanFieldModel(fieldName, newValue));
            }
        });
    }

    @Override
    public void setField(EntityModelWrapper entityModel, String fieldName) {
        this.entityModelWrapper = entityModel;
        this.fieldName = fieldName;

        Boolean boolValue = (Boolean) entityModel.getValue(fieldName).getValue();
        if(boolValue) {
            booleanEntityComboBox.setSelectedItem(ENTITY_TRUE);
        } else {
            booleanEntityComboBox.setSelectedItem(ENTITY_FALSE);
        }
    }
}
