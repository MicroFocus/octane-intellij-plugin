package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;

import javax.swing.*;

public abstract class FieldEditor extends JComponent {
    abstract void setField(EntityModelWrapper entityModelWrapper, String fieldName);
}
