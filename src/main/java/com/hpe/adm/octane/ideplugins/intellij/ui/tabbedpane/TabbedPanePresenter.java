package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.services.filtering.Filter;

import java.util.ArrayList;
import java.util.List;

public class TabbedPanePresenter implements Presenter<TabbedPaneView>{

    TabbedPaneView tabbedPaneView;

    List<EntityDetailPresenter> detailPresenters = new ArrayList<>();

    List<EntityTreeTablePresenter> treeTablePresenters = new ArrayList<>();

    public TabbedPanePresenter(){
        tabbedPaneView = new TabbedPaneView();
    }

    public void openMyWorkTab(String tabName){
        EntityTreeTablePresenter presenter = new EntityTreeTablePresenter();
        treeTablePresenters.add(presenter);
        tabbedPaneView.addTab(tabName, presenter.getView().getComponent());
    }

    public void openDetailTab(EntityModel entityModel){
        EntityDetailPresenter presenter = new EntityDetailPresenter();
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
        //test
        openMyWorkTab("My work");
        openDetailTab(null);
        return tabbedPaneView;
    }

    @Override
    public void setView(TabbedPaneView tabbedPaneView) {
        this.tabbedPaneView = tabbedPaneView;
    }

}
