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
import com.google.inject.Singleton;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.eventbus.OpenDetailTabEventListener;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
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
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.tabs.TabInfo;
import com.intellij.ui.tabs.TabsListener;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.swing.*;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.*;

@Singleton
public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    private static final Logger logger = Logger.getInstance(TabbedPanePresenter.class);

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

    @Inject
    private EntityIconFactory iconFactory;

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
    private Map<PartialEntity, EntityDetailPresenter> detailTabPresenterMap = new HashMap<>();

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private TabInfo selectedTabInfo;
    private TabInfo searchTabInfo;
    private TabInfo myWorkTabInfo;
    private Icon searchIcon = IconLoader.findIcon(Constants.IMG_SEARCH_ICON);

    public EntityTreeTablePresenter openMyWorkTab() {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        Icon myWorkIcon = IconLoader.findIcon(Constants.IMG_MYWORK);
        myWorkTabInfo = tabbedPaneView.addTab(Constants.TAB_MY_WORK_TITLE, null, myWorkIcon, presenter.getView().getComponent(), false);
        return presenter;
    }

    public void openSearchTab(String searchQuery) {
        String tabTitle = searchQuery;
        if (tabTitle.length() > 25) {
            tabTitle = Util.ellipsisTruncate(tabTitle, 25);
        }
        tabTitle = "\"" + tabTitle + "\"";

        //Only open one search tab
        if (searchTabInfo == null) {
            searchTabInfo = tabbedPaneView.addTab(
                    tabTitle,
                    null,
                    searchIcon,
                    entitySearchResultPresenter.getView().getComponent());

            tabbedPaneView.selectTabWithTabInfo(searchTabInfo, true);
        } else {
            searchTabInfo.setText(tabTitle);
            tabbedPaneView.selectTabWithTabInfo(searchTabInfo, true);
        }

        entitySearchResultPresenter.globalSearch(searchQuery);
    }

    public void openDetailTab(PartialEntity tabKey) {
        openDetailTab(tabKey.getEntityType(), tabKey.getEntityId(), tabKey.getEntityName());
    }

    public void openDetailTab(Entity entityType, Long entityId, String entityName) {
        PartialEntity partialEntity = new PartialEntity(entityId, entityName, entityType);
        if(isDetailTabAlreadyOpen(partialEntity)) {
            selectDetailTab(partialEntity);
        }
        else {
            EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
            presenter.setEntity(entityType, entityId);

            PartialEntity tabKey = new PartialEntity(entityId, entityName, entityType);
            ImageIcon tabIcon = new ImageIcon(iconFactory.getIconAsImage(entityType, 22, 11));

            TabInfo tabInfo = tabbedPaneView.addTab(
                    String.valueOf(entityId),
                    entityName,
                    tabIcon,
                    presenter.getView().getComponent());

            detailTabInfo.put(tabKey, tabInfo);
            detailTabPresenterMap.put(tabKey, presenter);

            saveDetailTabsToPersistentState();
            tabbedPaneView.selectTabWithTabInfo(tabInfo, true);
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
        EntityTreeTablePresenter myWorkPresenter = openMyWorkTab();

        //Init EntityTreeTablePresenter handlers
        myWorkPresenter.addEntityClickHandler((mouseEvent, entityType, entityId, model) -> {
            //double click
            if (SwingUtilities.isLeftMouseButton(mouseEvent) && mouseEvent.getClickCount() == 2) {
                onEntityAction(entityType, entityId, model, true);
            }

            //Middle click
            else if (SwingUtilities.isMiddleMouseButton(mouseEvent)) {
                onEntityAction(entityType, entityId, model, false);
            }
        });
        //Key handler
        myWorkPresenter.addEntityKeyHandler((event, entityType, entityId, model) -> {
            if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                onEntityAction(entityType, entityId, model, true);
            }
        });

        //Init EntitySearchResultPresenter handlers
        entitySearchResultPresenter.getView().addEntityMouseHandler((mouseEvent, entityType, entityId, model) -> {
            //double click
            if (SwingUtilities.isLeftMouseButton(mouseEvent) && mouseEvent.getClickCount() == 2) {
                onEntityAction(entityType, entityId, model, true);
            }

            //Middle click
            else if (SwingUtilities.isMiddleMouseButton(mouseEvent)) {
                onEntityAction(entityType, entityId, model, false);
            }
        });

        //Key handler
        entitySearchResultPresenter.getView().addEntityKeyHandler((keyEvent, entityType, entityId, model) -> {
            if (keyEvent.getKeyCode() == KeyEvent.VK_ENTER) {
                onEntityAction(entityType, entityId, model, true);
            }
        });

        // TODO: atoth, temporal coupling

        loadSearchHistory();

        // Make sure handler are init after history,
        // activating this handler b4 can overwrite the saved settings
        initHandlers();

        PartialEntity selectedTabKey = getSelectedTabToFromPersistentState();

        loadDetailTabsFromPersistentState();

        //attempt to reselect prev tab
        if (selectedTabKey != null) {
            selectDetailTab(selectedTabKey);
        }
    }

    private void initHandlers() {
        tabbedPaneView.setSearchRequestHandler(this::openSearchTab);

        //TODO atoth: should only save once at the end
        tabbedPaneView.addTabsListener(new TabsListener.Adapter() {
            @Override
            public void selectionChanged(TabInfo oldSelection, TabInfo newSelection) {
                logger.debug("Selection changed to: " + newSelection);
                saveSelectedTabToToPersistentState(detailTabInfo.inverse().get(newSelection));
                selectedTabInfo = newSelection;
            }

            @Override
            public void tabRemoved(TabInfo tabToRemove) {
                if (tabToRemove == searchTabInfo) {
                    searchTabInfo = null;
                }

                PartialEntity partialEntity = detailTabInfo.inverse().get(tabToRemove);

                if(detailTabPresenterMap.containsKey(partialEntity)) {
                    EntityDetailPresenter entityDetailPresenter = detailTabPresenterMap.get(partialEntity);
                    entityDetailPresenter.closing();
                    detailTabPresenterMap.remove(partialEntity);
                }

                saveDetailTabsToPersistentState();

                detailTabInfo.remove(partialEntity);
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
    private void onEntityAction(Entity entityType, Long entityId, EntityModel model, boolean selectNewTab) {

        //Convert if needed
        if (entityType == USER_ITEM) {
            model = MyWorkUtil.getEntityModelFromUserItem(model);
            entityType = Entity.getEntityType(model);
            entityId = Long.valueOf(model.getValue("id").getValue().toString());
        }

        //Special handling of comments
        if (Entity.COMMENT.equals(entityType)) {
            //Convert to parent of comment and continue
            model = (EntityModel) Util.getContainerItemForCommentModel(model).getValue();
            entityType = Entity.getEntityType(model);
            entityId = Long.valueOf(model.getValue("id").getValue().toString());
        }

        if (!isDetailTabSupported(entityType)) {
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

        if (isDetailTabAlreadyOpen(tabKey)) {
            if (selectNewTab) {
                selectDetailTab(tabKey);
            }
        } else {
            openDetailTab(entityType, entityId, entityName);
            if (selectNewTab) {
                selectDetailTab(tabKey);
            }
        }
    }

    public Presenter getSelectedPresenter() {
        if(isMyWorkSelected()) {
            return entityTreeTablePresenterProvider.get();
        } else if (isSearchTabSelected()) {
            return entitySearchResultPresenter;
        } else {
            return detailTabPresenterMap.get(detailTabInfo.inverse().get(selectedTabInfo));
        }
    }

    public static boolean isDetailTabSupported(Entity entityType) {
        return supportedDetailTabs.contains(entityType);
    }

    private void selectDetailTab(PartialEntity tabKey) {
        tabbedPaneView.selectTabWithTabInfo(detailTabInfo.get(tabKey), true);
    }

    private boolean isDetailTabAlreadyOpen(PartialEntity tabKey) {
        return detailTabInfo.containsKey(tabKey) && tabbedPaneView.hasTabWithTabInfo(detailTabInfo.get(tabKey));
    }

    private void loadDetailTabsFromPersistentState() {
        JSONObject jsonObject = idePluginPersistentState.loadState(IdePluginPersistentState.Key.OPEN_TABS);
        if (jsonObject == null) return;

        JSONArray jsonArray = jsonObject.getJSONArray("openDetailTabs");
        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject obj = jsonArray.getJSONObject(i);
            openDetailTab(PartialEntity.fromJsonObject(obj));
        }
    }

    private void saveDetailTabsToPersistentState() {
        JSONObject jsonObject = new JSONObject();
        JSONArray jsonArray = new JSONArray();

        tabbedPaneView.getTabInfos()
                .stream()
                .filter(detailTabInfo::containsValue)
                .forEach(tabInfo -> jsonArray.put(PartialEntity.toJsonObject(detailTabInfo.inverse().get(tabInfo))));

        jsonObject.put("openDetailTabs", jsonArray);
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.OPEN_TABS, jsonObject);
    }

    private void saveSelectedTabToToPersistentState(PartialEntity selectedTab) {
        //My work is null (not a detail tab) good enough for now
        if (selectedTab == null) {
            idePluginPersistentState.clearState(IdePluginPersistentState.Key.SELECTED_TAB);
        } else {
            idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_TAB, PartialEntity.toJsonObject(selectedTab));
        }
    }

    private PartialEntity getSelectedTabToFromPersistentState() {
        JSONObject jsonObject = idePluginPersistentState.loadState(IdePluginPersistentState.Key.SELECTED_TAB);
        if (jsonObject == null) {
            return null;
        } else {
            return PartialEntity.fromJsonObject(jsonObject);
        }
    }

    private void loadSearchHistory() {
        tabbedPaneView.setSearchHistory(searchManager.getSearchHistory());
    }

    public void selectMyWorkTab() {
        tabbedPaneView.selectTabWithTabInfo(myWorkTabInfo, true);
    }

    public boolean isMyWorkSelected() {
        return Objects.equals(myWorkTabInfo, selectedTabInfo);
    }

    public boolean isSearchTabSelected() {
        return Objects.equals(searchTabInfo, selectedTabInfo);
    }

    public boolean isDetailTabSelected() {
        return detailTabInfo.inverse().containsKey(selectedTabInfo);
    }

}