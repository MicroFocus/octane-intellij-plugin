package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.services.filtering.Filter;

import java.util.ArrayList;
import java.util.List;

public class TabbedPanePresenter implements Presenter<TabbedPaneView>{

    @Inject
    TabbedPaneView tabbedPaneView;

    @Inject
    Provider<EntityDetailPresenter> entityDetailPresenterProvider;
    @Inject
    Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;

    List<EntityDetailPresenter> detailPresenters = new ArrayList<>();
    List<EntityTreeTablePresenter> treeTablePresenters = new ArrayList<>();

    public void openMyWorkTab(String tabName){
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        treeTablePresenters.add(presenter);
        tabbedPaneView.addTab(tabName, presenter.getView().getComponent());
    }

    public void openDetailTab(EntityModel entityModel){
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        detailPresenters.add(presenter);
        tabbedPaneView.addTab("ETITTY NAME", presenter.getView().getComponent());
    }

    public void openSearchTab(String searchQuery){
        //TODO: impl me
    }

    public void openFilteredTab(String tabName, List<Filter> filters){
        //TODO: impl me
    }

    public TabbedPaneView getView(){
        return tabbedPaneView;
    }

    @Override
    @Inject
    public void setView(TabbedPaneView tabbedPaneView) {
        this.tabbedPaneView = tabbedPaneView;

        //open test tabs
        openMyWorkTab("My work");
        openDetailTab(null);
    }

}
