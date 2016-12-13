package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.tabs.TabInfo;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    @Inject
    TabbedPaneView tabbedPaneView;
    @Inject
    EntityService entityService;

    @Inject
    private Provider<EntityDetailPresenter> entityDetailPresenterProvider;
    @Inject
    private Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;

    private Map<String, TabInfo> detailTabInfo = new HashMap<>();

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(20,20,10,Color.WHITE);

    public EntityTreeTablePresenter openMyWorkTab() {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        Icon myWorkIcon = IconLoader.findIcon(Constants.IMG_MYWORK);
        tabbedPaneView.addTab(Constants.TAB_MY_WORK_TITLE, null, myWorkIcon , presenter.getView().getComponent(), false);
        return presenter;
    }

    public void openDetailTab(EntityModel entityModel) {
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        presenter.setEntity(entityModel);

        Entity entityType = Entity.getEntityType(entityModel);
        String entityName = entityModel.getValue("name").getValue().toString();
        String entityId = entityModel.getValue("id").getValue().toString();
        String tabId = entityType.name() + entityId;

        if(detailTabInfo.containsKey(tabId) && tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabId))){
            tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
        } else {

            ImageIcon tabIcon = new ImageIcon(entityIconFactory.getIconAsImage(entityType));

            TabInfo tabInfo = tabbedPaneView.addTab(
                    entityId,
                    entityName,
                    tabIcon,
                    presenter.getView().getComponent());

            detailTabInfo.put(tabId, tabInfo);
        }
    }

    public TabbedPaneView getView() {
        return tabbedPaneView;
    }

    @Override
    @Inject
    public void setView(TabbedPaneView tabbedPaneView) {
        this.tabbedPaneView = tabbedPaneView;

        //open test entity tree view
        EntityTreeTablePresenter presenter = openMyWorkTab();

        //TODO: atoth: entity should be reloaded in the future
        presenter.addEntityDoubleClickHandler((entityType, entityId, model) -> {
            openDetailTab(model); //would still work, but it will be partially loaded in the future
//            try {
//                openDetailTab(entityService.findEntity(entityType, entityId));
//            } catch (ServiceException ex){
//                ex.printStackTrace();
//            }
        });

    }
}
