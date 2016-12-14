package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.inject.Inject;
import com.google.inject.Provider;
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
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 10, Color.WHITE);
    @Inject
    TabbedPaneView tabbedPaneView;
    @Inject
    EntityService entityService;
    @Inject
    private Provider<EntityDetailPresenter> entityDetailPresenterProvider;
    @Inject
    private Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;
    private Map<String, TabInfo> detailTabInfo = new HashMap<>();

    public EntityTreeTablePresenter openMyWorkTab() {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        Icon myWorkIcon = IconLoader.findIcon(Constants.IMG_MYWORK);
        tabbedPaneView.addTab(Constants.TAB_MY_WORK_TITLE, null, myWorkIcon, presenter.getView().getComponent(), false);
        return presenter;
    }

    public void openDetailTab(Entity entityType, Long entityId, String entityName) {
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        presenter.setEntity(entityType, entityId);
        String tabId = entityType.name() + entityId;
        ImageIcon tabIcon = new ImageIcon(entityIconFactory.getIconAsImage(entityType));

        TabInfo tabInfo = tabbedPaneView.addTab(
                String.valueOf(entityId),
                entityName,
                tabIcon,
                presenter.getView().getComponent());

        detailTabInfo.put(tabId, tabInfo);
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
        initHandlers(presenter);
    }

    private void initHandlers(EntityTreeTablePresenter presenter){
        presenter.addEntityClickHandler((mouseEvent, entityType, entityId, model) -> {

            //Need the name for the tab tooltip
            String entityName = model.getValue("name").getValue().toString();
            String tabId = entityType.name() + entityId;

            //double click
            if(SwingUtilities.isLeftMouseButton(mouseEvent) && mouseEvent.getClickCount() == 2){
                if (detailTabInfo.containsKey(tabId) && tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabId))) {
                    tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
                } else {
                    openDetailTab(entityType, entityId, entityName);
                    tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
                }
            }
            //Middle click
            else if(SwingUtilities.isMiddleMouseButton(mouseEvent)){
                if (!detailTabInfo.containsKey(tabId) && !tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabId))) {
                    openDetailTab(entityType, entityId, entityName);
                }
            }
        });
        presenter.addEntityKeyHandler((event, selectedEntityType, selectedEntityId, model) -> {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                //Need the name for the tab tooltip
                String entityName = model.getValue("name").getValue().toString();
                String tabId = selectedEntityType.name() + selectedEntityId;

                if (detailTabInfo.containsKey(tabId) && tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabId))) {
                    tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
                } else {
                    openDetailTab(selectedEntityType, selectedEntityId, entityName);
                    tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
                }
            }
        });
    }
}

