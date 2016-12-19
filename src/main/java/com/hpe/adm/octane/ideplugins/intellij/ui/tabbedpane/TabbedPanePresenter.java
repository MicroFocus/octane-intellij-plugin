package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
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
        String tabId = createTabId(entityType, entityId);

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
                if(isDetailTabAlreadyOpen(tabId)){
                    selectDetailTab(tabId);
                } else {
                    openDetailTab(entityType, entityId, entityName);
                    selectDetailTab(tabId);
                }
            }

            //Middle click
            else if(SwingUtilities.isMiddleMouseButton(mouseEvent)){
                if(!isDetailTabAlreadyOpen(tabId)){
                    openDetailTab(entityType, entityId, entityName);
                }
            }
        });

        presenter.addEntityKeyHandler((event, selectedEntityType, selectedEntityId, model) -> {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                //Need the name for the tab tooltip
                String entityName = model.getValue("name").getValue().toString();
                String tabId = createTabId(selectedEntityType, selectedEntityId);

                if(isDetailTabAlreadyOpen(tabId)){
                    selectDetailTab(tabId);
                } else {
                    openDetailTab(selectedEntityType, selectedEntityId, entityName);
                    selectDetailTab(tabId);
                }
            }
        });
    }

    private void selectDetailTab(String tabId){
        tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
    }

    /**
     * Create an ID that identifies what entity detail tabs you have open, used as a mapkey
     * Use {@link #isDetailTabAlreadyOpen(String tabId)} or {@link #isDetailTabAlreadyOpen(Entity entityType, Long entityId)}
     * to see if a tab is already open
     * @param entityType
     * @param entityId
     * @return a unique key for a detail tab
     */
    private String createTabId(Entity entityType, Long entityId){
        return entityType.name() + entityId;
    }

    public boolean isDetailTabAlreadyOpen(Entity entityType, Long entityId){
        return isDetailTabAlreadyOpen(createTabId(entityType, entityId));
    }

    private boolean isDetailTabAlreadyOpen(String tabId){
        return detailTabInfo.containsKey(tabId) && tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabId));
    }
}

