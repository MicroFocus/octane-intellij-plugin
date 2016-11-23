package com.hpe.adm.octane.ideplugins.intellij.ui.main;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;

public class MainPresenter implements Presenter<MainView> {

    MainView mainView;

    TabbedPanePresenter tabbedPanePresenter;

    @Inject
    public MainPresenter(TabbedPanePresenter tabbedPanePresenter){
        this.tabbedPanePresenter = tabbedPanePresenter;
    }

    @Override
    public MainView getView() {
        return mainView;
    }

    @Override
    @Inject
    public void setView(MainView view) {
        this.mainView = view;
        view.setTabView(tabbedPanePresenter.getView());
    }

}
