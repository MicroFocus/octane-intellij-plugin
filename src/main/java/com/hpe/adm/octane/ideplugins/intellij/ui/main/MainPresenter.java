package com.hpe.adm.octane.ideplugins.intellij.ui.main;

import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane.TabbedPaneView;

public class MainPresenter implements Presenter<MainView> {

    MainView mainView;

    Presenter<TabbedPaneView> tabbedPanePresenter;

    public MainPresenter() {
    }

    public MainPresenter(Presenter<TabbedPaneView> tabbedPanePresenter){
        this.tabbedPanePresenter = tabbedPanePresenter;
    }

    @Override
    public MainView getView() {
        return mainView;
    }

    @Override
    public void setView(MainView view) {
        this.mainView = view;
        view.setTabView(tabbedPanePresenter.getView());
    }

}
