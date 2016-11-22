package com.hpe.adm.octane.ideplugins.intellij.ui;

public interface Presenter<V extends View> {
    V getView();
    void setView(V view);
}
