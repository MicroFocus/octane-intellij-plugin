package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.inject.Inject;
import com.google.inject.Provider;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentSettings;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.util.Pair;
import com.intellij.ui.tabs.TabInfo;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;

public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(20, 20, 10, Color.WHITE);

    @Inject
    TabbedPaneView tabbedPaneView;

    @Inject
    private Provider<EntityDetailPresenter> entityDetailPresenterProvider;
    @Inject
    private Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;

    @Inject
    private IdePersistentSettings settings;

    private BiMap<String, TabInfo> detailTabInfo = HashBiMap.create();

    public EntityTreeTablePresenter openMyWorkTab() {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        Icon myWorkIcon = IconLoader.findIcon(Constants.IMG_MYWORK);
        tabbedPaneView.addTab(Constants.TAB_MY_WORK_TITLE, null, myWorkIcon, presenter.getView().getComponent(), false);
        return presenter;
    }

    public void openDetailTab(Entity entityType, Long entityId, String entityName) {
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        presenter.setEntity(entityType, entityId);
        String tabId = getDetailTabKey(entityType, entityId);
        ImageIcon tabIcon = new ImageIcon(entityIconFactory.getIconAsImage(entityType));

        TabInfo tabInfo = tabbedPaneView.addTab(
                String.valueOf(entityId),
                entityName,
                tabIcon,
                presenter.getView().getComponent());

        detailTabInfo.put(tabId, tabInfo);
        syncOpenDetailTabsWithSettings();
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
        openSavedDetailTabs();
    }

    private void initHandlers(EntityTreeTablePresenter presenter){
        presenter.addEntityClickHandler((mouseEvent, entityType, entityId, model) -> {

            //Need the name for the tab tooltip
            String entityName = model.getValue("name").getValue().toString();
            String tabId = getDetailTabKey(entityType, entityId);

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

        //Enter key
        presenter.addEntityKeyHandler((event, selectedEntityType, selectedEntityId, model) -> {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                //Need the name for the tab tooltip
                String entityName = model.getValue("name").getValue().toString();
                String tabId = getDetailTabKey(selectedEntityType, selectedEntityId);

                if (detailTabInfo.containsKey(tabId) && tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabId))) {
                    tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
                } else {
                    openDetailTab(selectedEntityType, selectedEntityId, entityName);
                    tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabId), false);
                }
            }
        });
    }

    /**
     * TODO atoth, this is a really inefficient way to do things
     */
    private void syncOpenDetailTabsWithSettings(){
        JSONArray jsonArray = new JSONArray();
        for(TabInfo tabInfo : tabbedPaneView.getTabInfos()){
            String tabKey = detailTabInfo.inverse().get(tabInfo);
            if(tabKey!=null) {
                jsonArray.put(new JSONObject(tabKey));
            }
        }

        JSONObject jsonObject = new JSONObject();
        jsonObject.put("tabs", jsonArray);
        settings.setSetting(IdePersistentSettings.Key.OPEN_TABS, jsonObject);
    }

    private void openSavedDetailTabs(){
        JSONObject jsonObject = settings.getSetting(IdePersistentSettings.Key.OPEN_TABS);
        if(jsonObject!=null){
            JSONArray array = jsonObject.getJSONArray("tabs");
            for (int i = 0; i < array.length(); i++) {
                JSONObject obj = array.getJSONObject(i);
                Pair<Entity, Long> pair = getDataFromTabKey(obj);
                openDetailTab(pair.first, pair.second, "IDUNNO");
            }
        }
    }

    private String getDetailTabKey(Entity entityType, Long entityId){
        JSONObject json = new JSONObject();
        json.put("type", entityType.name());
        json.put("id", entityId);
        return json.toString();
    }

    private Pair<Entity, Long> getDataFromTabKey(JSONObject jsonObject){
        Entity entity = Entity.valueOf(jsonObject.getString("type"));
        Long id = jsonObject.getLong("id");
        return Pair.create(entity,id);
    }


}

