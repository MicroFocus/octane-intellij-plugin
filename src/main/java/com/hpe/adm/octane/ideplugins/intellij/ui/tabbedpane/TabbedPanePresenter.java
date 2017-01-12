package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableSet;
import com.google.inject.Inject;
import com.google.inject.Provider;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.EntitySearchResultPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.tabs.TabInfo;
import com.intellij.ui.tabs.TabsListener;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;

public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    // TODO to be kept up-to-date
    public static ImmutableSet<Entity> supportedDetailTabs;
    static {
        supportedDetailTabs = ImmutableSet.copyOf(new Entity[]{
                Entity.USER_STORY,
                Entity.DEFECT,
                Entity.TASK,
                Entity.GHERKIN_TEST,
                Entity.MANUAL_TEST
        });
    }

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(25, 25, 12, Color.WHITE);

    @Inject
    TabbedPaneView tabbedPaneView;

    @Inject
    private Provider<EntityDetailPresenter> entityDetailPresenterProvider;

    @Inject
    private Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;

    @Inject
    private EntitySearchResultPresenter entitySearchResultPresenter;

    @Inject
    private EntityService entityService;

    private BiMap<PartialEntity, TabInfo> detailTabInfo = HashBiMap.create();

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private TabInfo searchTab;
    private Icon searchIcon = IconLoader.findIcon("/com/intellij/ide/ui/laf/icons/search.png");

    public EntityTreeTablePresenter openMyWorkTab() {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        Icon myWorkIcon = IconLoader.findIcon(Constants.IMG_MYWORK);
        tabbedPaneView.addTab(Constants.TAB_MY_WORK_TITLE, null, myWorkIcon, presenter.getView().getComponent(), false);
        return presenter;
    }

    public void openSearchTab(String searchQuery) {
        //Only open one search tab
        if(searchTab==null) {
            searchTab = tabbedPaneView.addTab(
                    "\"" + searchQuery + "\"",
                    null,
                    searchIcon,
                    entitySearchResultPresenter.getView().getComponent());

            tabbedPaneView.selectTabWithTabInfo(searchTab, true);
        } else {
            searchTab.setText("\"" + searchQuery + "\"");
        }

        entitySearchResultPresenter.globalSearch(searchQuery);
    }

    public void openDetailTab(PartialEntity tabKey) {
        openDetailTab(tabKey.getEntityType(), tabKey.getEntityId(), tabKey.getEntityName());
    }

    public void openDetailTab(Entity entityType, Long entityId, String entityName) {
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        presenter.setEntity(entityType, entityId);
        PartialEntity tabKey = new PartialEntity(entityId, entityName, entityType);

        ImageIcon tabIcon = new ImageIcon(entityIconFactory.getIconAsImage(entityType));

        TabInfo tabInfo = tabbedPaneView.addTab(
                String.valueOf(entityId),
                entityName,
                tabIcon,
                presenter.getView().getComponent());

        detailTabInfo.put(tabKey, tabInfo);

        saveDetailTabsToPersistentState();
    }

    public TabbedPaneView getView() {
        return tabbedPaneView;
    }

    @Override
    @Inject
    public void setView(TabbedPaneView tabbedPaneView) {
        this.tabbedPaneView = tabbedPaneView;

        //Init tabbed pane view handlers
        initHandlers();

        //open test entity tree view
        EntityTreeTablePresenter entityTreeTablePresenter = openMyWorkTab();

        //Init EntityTreeTablePresenter handlers
        entityTreeTablePresenter.addEntityClickHandler((mouseEvent, entityType, entityId, model) -> {
            //double click
            if(SwingUtilities.isLeftMouseButton(mouseEvent) && mouseEvent.getClickCount() == 2){
                onEntityAction(entityType, entityId, model, true);
            }

            //Middle click
            else if(SwingUtilities.isMiddleMouseButton(mouseEvent)){
                onEntityAction(entityType, entityId, model, false);
            }
        });
        //Key handler
        entityTreeTablePresenter.addEntityKeyHandler((event, entityType, entityId, model) -> {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                onEntityAction(entityType, entityId, model, true);
            }
        });

        //Init EntitySearchResultPresenter handlers
        entitySearchResultPresenter.getView().addEntityMouseHandler((mouseEvent, entityType, entityId, model) -> {
            //double click
            if(SwingUtilities.isLeftMouseButton(mouseEvent) && mouseEvent.getClickCount() == 2){
                onEntityAction(entityType, entityId, model, true);
            }

            //Middle click
            else if(SwingUtilities.isMiddleMouseButton(mouseEvent)){
                onEntityAction(entityType, entityId, model, false);
            }
        });

        //Key handler
        entitySearchResultPresenter.getView().addEntityKeyHandler((keyEvent, entityType, entityId, model) -> {
            if (keyEvent.getKeyCode() == KeyEvent.VK_ENTER) {
                onEntityAction(entityType, entityId, model, true);
            }
        });

        //Persistence
        loadDetailTabsFromPersistentState();
        selectSelectedTabToFromPersistentState();
    }

    private void initHandlers(){
        tabbedPaneView.setSearchRequestHandler(query -> openSearchTab(query));

        //TODO atoth: should only save once at the end
        tabbedPaneView.addTabsListener(new TabsListener.Adapter() {
            @Override
            public void selectionChanged(TabInfo oldSelection, TabInfo newSelection) {
                saveSelectedTabToToPersistentState(detailTabInfo.inverse().get(newSelection));
            }
            @Override
            public void tabRemoved(TabInfo tabToRemove) {
                if(tabToRemove == searchTab){
                    searchTab = null;
                }
                saveDetailTabsToPersistentState();
            }
            @Override
            public void tabsMoved() {
                saveDetailTabsToPersistentState();
            }
        });
    }

    /**
     * Handle key and mouse events
     */
    private void onEntityAction(Entity entityType, Long entityId, EntityModel model, boolean selectNewTab){
        //Special handling of comments
        if(Entity.COMMENT.equals(entityType)){
            //Convert to parent of comment and continue
            model = (EntityModel) UiUtil.getContainerItemForCommentModel(model).getValue();
            entityType = Entity.getEntityType(model);
            entityId = Long.valueOf(model.getValue("id").getValue().toString());
        }

        if(!isDetailTabSupported(entityType)){
            entityService.openInBrowser(model);
            return;
        }

        //Need the name for the tab tooltip
        String entityName = model.getValue("name").getValue().toString();
        PartialEntity tabKey = new PartialEntity(entityId, entityName, entityType);

        if(isDetailTabAlreadyOpen(tabKey)){
            if(selectNewTab) {
                selectDetailTab(tabKey);
            }
        } else {
            openDetailTab(entityType, entityId, entityName);
            if(selectNewTab) {
                selectDetailTab(tabKey);
            }
        }
    }

    private boolean isDetailTabSupported(Entity entityType) {
        return supportedDetailTabs.contains(entityType);
    }

    private void selectDetailTab(PartialEntity tabKey){
        tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabKey), false);
    }

    private boolean isDetailTabAlreadyOpen(PartialEntity tabKey){
        return detailTabInfo.containsKey(tabKey) && tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabKey));
    }

    private void loadDetailTabsFromPersistentState(){
        JSONObject jsonObject =  idePluginPersistentState.loadState(IdePluginPersistentState.Key.OPEN_TABS);
        if(jsonObject == null) return;

        JSONArray jsonArray = jsonObject.getJSONArray("openDetailTabs");
        for(int i = 0; i<jsonArray.length(); i++){
            JSONObject obj = jsonArray.getJSONObject(i);
            openDetailTab(PartialEntity.fromJsonObject(obj));
        }
    }

    private void saveDetailTabsToPersistentState(){
        JSONObject jsonObject = new JSONObject();
        JSONArray jsonArray = new JSONArray();

        tabbedPaneView.getTabInfos()
                .stream()
                .filter(detailTabInfo::containsValue)
                .forEach(tabInfo -> jsonArray.put(PartialEntity.toJsonObject(detailTabInfo.inverse().get(tabInfo))));

        jsonObject.put("openDetailTabs", jsonArray);
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.OPEN_TABS, jsonObject);
    }

    private void saveSelectedTabToToPersistentState(PartialEntity selectedTab){
        //My work is null (not a detail tab) good enough for now
        if(selectedTab==null){
            idePluginPersistentState.clearState(IdePluginPersistentState.Key.SELECTED_TAB);
        } else {
            idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_TAB, PartialEntity.toJsonObject(selectedTab));
        }
    }

    private void selectSelectedTabToFromPersistentState(){
        JSONObject jsonObject =  idePluginPersistentState.loadState(IdePluginPersistentState.Key.SELECTED_TAB);
        if(jsonObject == null) return;
        PartialEntity selectedTabKey = PartialEntity.fromJsonObject(jsonObject);
        if(detailTabInfo.containsKey(selectedTabKey)){
            selectDetailTab(selectedTabKey);
        }
    }

}