package com.hpe.adm.octane.ideplugins.intellij.ui;

public interface View<P extends Presenter> extends HasComponent{
    P getPresenter();
    void setPresenter(P presenter);
}
