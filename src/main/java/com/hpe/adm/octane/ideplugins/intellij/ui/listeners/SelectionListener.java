package com.hpe.adm.octane.ideplugins.intellij.ui.listeners;

import com.hpe.adm.octane.ideplugins.intellij.ui.detail.FieldsSelectPopup;

import java.util.EventListener;

public interface SelectionListener extends EventListener {
    void valueChanged(SelectionEvent e);
}