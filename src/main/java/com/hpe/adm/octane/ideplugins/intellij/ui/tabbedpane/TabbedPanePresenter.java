/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.eventbus.EventBus;
import com.google.inject.Inject;
import com.google.inject.Provider;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.OpenDetailTabEventListener;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.ToolbarActiveItem;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.EntitySearchResultPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.SearchHistoryManager;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.mywork.MyWorkUtil;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.tabs.TabInfo;
import com.intellij.ui.tabs.TabsListener;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.*;

public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    // TODO to be kept up-to-date
    public static ImmutableSet<Entity> supportedDetailTabs;
    static {
        supportedDetailTabs = ImmutableSet.copyOf(new Entity[]{
                USER_STORY,
                QUALITY_STORY,
                DEFECT,
                TASK,
                GHERKIN_TEST,
                MANUAL_TEST,
                MANUAL_TEST_RUN,
                TEST_SUITE_RUN,
                REQUIREMENT
        });
    }

    private static EntityIconFactory entityIconFactory = new EntityIconFactory(22, 22, 12, Color.WHITE);

    @Inject
    private SearchHistoryManager searchManager;

    @Inject
    private TabbedPaneView tabbedPaneView;

    @Inject
    private EventBus eventBus;

    @Inject
    private Provider<EntityDetailPresenter> entityDetailPresenterProvider;

    @Inject
    private Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;

    @Inject
    private EntitySearchResultPresenter entitySearchResultPresenter;

    @Inject
    private EntityService entityService;

    @Inject
    private Project project;

    private BiMap<PartialEntity, TabInfo> detailTabInfo = HashBiMap.create();

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private TabInfo searchTab;
    private Icon searchIcon = IconLoader.findIcon("/com/intellij/ide/ui/laf/icons/search.png");

    private Map<Entity,List<EntityDetailView>> presenters = new HashMap<>();

    public EntityTreeTablePresenter openMyWorkTab() {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        Icon myWorkIcon = IconLoader.findIcon(Constants.IMG_MYWORK);
        tabbedPaneView.addTab(Constants.TAB_MY_WORK_TITLE, null, myWorkIcon, presenter.getView().getComponent(), false);

        return presenter;
    }

    public void openSearchTab(String searchQuery) {
        String tabTitle = searchQuery;
        if(tabTitle.length() > 25){
            tabTitle = Util.ellipsisTruncate(tabTitle, 25);
        }
        tabTitle = "\"" + tabTitle + "\"";

        //Only open one search tab
        if(searchTab==null) {
            searchTab = tabbedPaneView.addTab(
                    tabTitle,
                    null,
                    searchIcon,
                    entitySearchResultPresenter.getView().getComponent());

            tabbedPaneView.selectTabWithTabInfo(searchTab, true);
        } else {
            searchTab.setText(tabTitle);
            tabbedPaneView.selectTabWithTabInfo(searchTab, true);
        }

        entitySearchResultPresenter.globalSearch(searchQuery);

        searchManager.saveSearchHistory();
    }

    public void openDetailTab(PartialEntity tabKey) {
        openDetailTab(tabKey.getEntityType(), tabKey.getEntityId(), tabKey.getEntityName());
    }

    public void openDetailTab(Entity entityType, Long entityId, String entityName) {
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        presenter.setEntity(entityType, entityId);
        if(presenters.containsKey(entityType)){
            presenters.get(entityType).add(presenter.getView());
        } else {
            List<EntityDetailView> detailViews = new ArrayList<>();
            detailViews.add(presenter.getView());
            presenters.put(entityType,detailViews);
        }
        presenter.getView().addFieldSelectListener(e -> {
                                                        presenters.get(entityType).remove(presenter.getView());
                                                        presenters.get(entityType).forEach(f -> f.setSelectedFields(presenter.getView().getSelectedFields()));
                                                        presenters.get(entityType).add(presenter.getView());});
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

        //we only need to load the search history at the beginning
        searchManager.loadSearchHistory();

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

        //Open and select active item on toolbar action click
        ToolbarActiveItem.setActiveItemClickHandler(project, ()->{
            JSONObject jsonObject = idePluginPersistentState.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            if(jsonObject != null){
                PartialEntity activeItem = PartialEntity.fromJsonObject(jsonObject);
                if(!isDetailTabAlreadyOpen(activeItem)){
                    openDetailTab(activeItem.getEntityType(), activeItem.getEntityId(), activeItem.getEntityName());
                }
                selectDetailTab(activeItem);
            }
        });

        //Persistence
        PartialEntity selectedTabKey = getselectedTabToFromPersistentState();

        loadDetailTabsFromPersistentState();

        //attempt to reselect prev tab
        if(selectedTabKey!=null) {
            selectDetailTab(selectedTabKey);
        }

        loadSearchHistory();

        // Make sure handler are init after history,
        // activating this handler b4 can overwrite the saved settings
        initHandlers();
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

        //Open tabs from the event bus
        eventBus.register((OpenDetailTabEventListener) openDetailTabEvent -> {
            EntityModel entityModel = openDetailTabEvent.getEntityModel();
            TabbedPanePresenter.this.onEntityAction(
                    Entity.getEntityType(entityModel),
                    Long.parseLong(entityModel.getValue("id").getValue().toString()),
                    entityModel,
                    true);
        });
    }

    /**
     * Handle key and mouse events
     */
    private void onEntityAction(Entity entityType, Long entityId, EntityModel model, boolean selectNewTab){

        //Convert if needed
        if(entityType == USER_ITEM) {
            model = MyWorkUtil.getEntityModelFromUserItem(model);
            entityType = Entity.getEntityType(model);
            entityId = Long.valueOf(model.getValue("id").getValue().toString());
        }

        //Special handling of comments
        if(Entity.COMMENT.equals(entityType)){
            //Convert to parent of comment and continue
            model = (EntityModel) Util.getContainerItemForCommentModel(model).getValue();
            entityType = Entity.getEntityType(model);
            entityId = Long.valueOf(model.getValue("id").getValue().toString());
        }

        if(!isDetailTabSupported(entityType)){
            //Show warning message
            Notification notification =
                    new Notification(
                            "Octane IntelliJ Plugin",
                            "Detail tab not supported",
                            "Opening " + entityType.name().toLowerCase() + " " + entityId + "  in browser...",
                            NotificationType.WARNING, null);

            notification.notify(project);

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

    public static boolean isDetailTabSupported(Entity entityType) {
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

    private PartialEntity getselectedTabToFromPersistentState(){
        JSONObject jsonObject =  idePluginPersistentState.loadState(IdePluginPersistentState.Key.SELECTED_TAB);
        if(jsonObject == null){
            return null;
        } else {
            PartialEntity selectedTabKey = PartialEntity.fromJsonObject(jsonObject);
            return selectedTabKey;
        }
    }



    private void loadSearchHistory(){
        tabbedPaneView.setSearchHistory(searchManager.getSearchHistory());
    }

}