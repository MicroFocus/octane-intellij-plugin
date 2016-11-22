package com.hpe.adm.octane.ideplugins.intellij.ui.main;

import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPanePresenter;

public class MainPresenter implements Presenter<MainView> {

    MainView mainView;
    TabbedPanePresenter tabbedPanePresenter;

    public MainPresenter() {
        mainView = new MainView(this);
        tabbedPanePresenter = new TabbedPanePresenter();
        mainView.setTabView(tabbedPanePresenter.getView());
    }

    @Override
    public MainView getView() {
        return mainView;
    }

    @Override
    public void setView(MainView view) {
        this.mainView = view;
    }

}
