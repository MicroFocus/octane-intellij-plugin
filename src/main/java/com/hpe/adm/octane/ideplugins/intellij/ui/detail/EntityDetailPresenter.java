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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.authentication.SimpleUserAuthentication;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.CommentService;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.MetadataService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.connection.ExposedGoogleHttpClient;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.vcs.VcsShowConfirmationOption;
import com.intellij.util.ui.ConfirmationDialog;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.stream.Collectors;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.*;

public class EntityDetailPresenter implements Presenter<EntityDetailView> {

    @Inject
    private EntityService entityService;
    @Inject
    private CommentService commentService;
    @Inject
    private Project project;
    @Inject
    private MetadataService metadataService;

    private EntityDetailView entityDetailView;
    private Entity entityType;
    private Long entityId;
    private Collection<FieldMetadata> fields;
    private EntityModel entityModel;
    private PluginModule pluginModule;
    private Logger logger = Logger.getInstance("EntityDetailPresenter");
    private final String GO_TO_BROWSER_DIALOG_MESSAGE = "\nYou can only provide a value for this field using ALM Octane in a browser." + "\nDo you want to do this now? ";

    public EntityDetailPresenter() {
    }

    public EntityDetailView getView() {
        return entityDetailView;
    }

    @Override
    @Inject
    public void setView(EntityDetailView entityDetailView) {
        this.entityDetailView = entityDetailView;
    }

    public void setEntity(Entity entityType, Long entityId) {
        this.entityType = entityType;
        this.entityId = entityId;

        //obtain information regarding the current project and the pluginmodule
        DataContext dataContext = DataManager.getInstance().getDataContextFromFocus().getResult();
        Project project = DataKeys.PROJECT.getData(dataContext);
        pluginModule = PluginModule.getPluginModuleForProject(project);

        RestUtil.runInBackground(
                () -> {
                    try {
                        fields = metadataService.getVisibleFields(entityType);

                        Set<String> requestedFields = fields.stream().map(FieldMetadata::getName).collect(Collectors.toSet());
                        entityModel = entityService.findEntity(this.entityType, this.entityId, requestedFields);

                        //The subtype field is absolutely necessary, yet the server sometimes has weird ideas, and doesn't return it
                        if(entityType.isSubtype()){
                            entityModel.setValue(new StringFieldModel(DetailsViewDefaultFields.FIELD_SUBTYPE, entityType.getSubtypeName()));
                        }

                        return entityModel;
                    } catch (ServiceException ex) {
                        entityDetailView.setErrorMessage(ex.getMessage());
                        return null;
                    }
                },
                (entityModel) -> {
                    if (entityModel != null) {
                        setUpCookie();
                        this.entityModel = entityModel;
                        entityDetailView.createDetailsPanel(entityModel, fields, pluginModule);
                        entityDetailView.setSaveSelectedPhaseButton(new SaveSelectedPhaseAction());
                        entityDetailView.setRefreshEntityButton(new EntityRefreshAction());
                        if (entityType != TASK) {
                            entityDetailView.setCommentsEntityButton(new EntityCommentsAction());
                            setComments(entityModel);
                            addSendNewCommentAction(entityModel);
                        }
                        if (entityType != MANUAL_TEST_RUN && entityType != TEST_SUITE_RUN) {
                            setPossibleTransitions(entityModel);
                            entityDetailView.setPhaseInHeader(true);
                        } else {
                            entityDetailView.removeSaveSelectedPhaseButton();
                            entityDetailView.setPhaseInHeader(false);
                        }
                        entityDetailView.setFieldSelectButton(new SelectFieldsAction());
                        //Title goes to browser
                        entityDetailView.setEntityNameClickHandler(() -> entityService.openInBrowser(entityModel));
                    }
                },
                null,
                null,
                "Loading entity " + entityType.name() + ": " + entityId);


    }

    private void setPossibleTransitions(EntityModel entityModel) {
        Set<EntityModel> result = new HashSet<>();
        RestUtil.runInBackground(() -> {
            String currentPhaseId = Util.getUiDataFromModel(entityModel.getValue("phase"), "id");
            return entityService.findPossibleTransitionFromCurrentPhase(Entity.getEntityType(entityModel), currentPhaseId);
        }, (possibleTransitions) -> {
            if (possibleTransitions.isEmpty()) {
                possibleTransitions.add(new EntityModel("target_phase", "No transition"));
                entityDetailView.setPossiblePhasesForEntity(possibleTransitions);
                entityDetailView.removeSaveSelectedPhaseButton();
            } else {
                entityDetailView.setPossiblePhasesForEntity(possibleTransitions);
            }
        }, null, "Failed to get possible transitions", "fetching possible transitions");
    }

    private void setComments(EntityModel entityModel) {
        Collection<EntityModel> result = new HashSet<>();
        RestUtil.runInBackground(() -> commentService.getComments(entityModel), (comments) -> entityDetailView.setComments(comments), null, "Failed to get possible comments", "fetching comments");
    }

    private final class EntityRefreshAction extends AnAction {
        public EntityRefreshAction() {
            super("Refresh current entity", "Refresh entity details", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            entityDetailView.doRefresh();
            setEntity(entityType, entityId);
        }
    }

    private final class EntityCommentsAction extends AnAction {
        public EntityCommentsAction() {
            super("Show comments for current entity", "Show comments for current entity", IconLoader.findIcon(Constants.IMG_COMMENTS_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            entityDetailView.getEntityDetailsPanel().activateCommentsCollapsible();

        }
    }

    public final class SelectFieldsAction extends AnAction {

        private boolean defaultfields = true;

        public void setDefaultFieldsIcon(boolean defaultfields) {
            this.defaultfields = defaultfields;
        }

        public SelectFieldsAction() {
            super("Select fields for this entity type", "Select fields popup", IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT));
        }

        public void actionPerformed(AnActionEvent e) {
            entityDetailView.getEntityDetailsPanel().activateFieldsSettings();
        }

        public void update(AnActionEvent e) {
            if (defaultfields) {
                e.getPresentation().setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT));
            } else {
                e.getPresentation().setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_NON_DEFAULT));
            }
        }
    }

    private final class SaveSelectedPhaseAction extends AnAction {
        public SaveSelectedPhaseAction() {
            super("Save selected phase", "Save changes to entity phase", IconLoader.findIcon("/actions/menu-saveall.png"));
        }

        public void actionPerformed(AnActionEvent e) {
            RestUtil.runInBackground(() -> {
                EntityModel selectedTransition = entityDetailView.getSelectedTransition();
                return (ReferenceFieldModel) selectedTransition.getValue("target_phase");
            }, (nextPhase) -> {
                try {
                    entityService.updateEntityPhase(entityDetailView.getEntityModel(), nextPhase);
                } catch (OctaneException ex) {
                    if (ex.getMessage().contains("400")) {
                        String errorMessage = "Failed to change phase";
                        try {
                            JsonParser jsonParser = new JsonParser();
                            JsonObject jsonObject = (JsonObject) jsonParser.parse(ex.getMessage().substring(ex.getMessage().indexOf("{")));
                            errorMessage = jsonObject.get("description_translated").getAsString();
                        } catch (Exception e1) {
                            logger.debug("Failed to get JSON message from Octane Server" + e1.getMessage());
                        }
                        ConfirmationDialog dialog = new ConfirmationDialog(
                                project,
                                "Server message: " + errorMessage + GO_TO_BROWSER_DIALOG_MESSAGE,
                                "Business rule violation",
                                null, VcsShowConfirmationOption.STATIC_SHOW_CONFIRMATION) {
                            @Override
                            public void setDoNotAskOption(@Nullable DoNotAskOption doNotAsk) {
                                super.setDoNotAskOption(null);
                            }
                        };
                        if (dialog.showAndGet()) {
                            entityService.openInBrowser(entityModel);
                        }
                    }
                }
                entityDetailView.doRefresh();
                setEntity(entityType, entityId);
            }, null, "Failed to move to next phase", "Moving to next phase");

        }
    }

    public void addSendNewCommentAction(EntityModel entityModel) {
        entityDetailView.addSendNewCommentAction(e -> {
            commentService.postComment(entityModel, entityDetailView.getCommentMessageBoxText());
            entityDetailView.setCommentMessageBoxText("");
            setComments(entityModel);
        });
    }

    private void setUpCookie(){

        ConnectionSettings connectionSettings = pluginModule.getInstance(ConnectionSettingsProvider.class).getConnectionSettings();
        String lwssoValue = getCookie(connectionSettings);
        //add the cookie to the CookieHandler, it will be used by webView
        Map<String, java.util.List<String>> headers = new LinkedHashMap<String, List<String>>();
        headers.put("Set-Cookie", Arrays.asList(lwssoValue));
        try {
            if( java.net.CookieHandler.getDefault() == null){
                CookieManager cookie_manager = new CookieManager();
                CookieHandler.setDefault(cookie_manager);
            }
            java.net.CookieHandler.getDefault().put(new URI(connectionSettings.getBaseUrl()), headers);
        } catch (IOException e) {
            e.printStackTrace();
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }

    private String getCookie(ConnectionSettings connectionSettings){
        ExposedGoogleHttpClient client = new ExposedGoogleHttpClient(connectionSettings.getBaseUrl());
        SimpleUserAuthentication userAuthentication = new SimpleUserAuthentication(connectionSettings.getUserName(),
                connectionSettings.getPassword());
        client.authenticate(userAuthentication);
        String lwssoValue = "LWSSO_COOKIE_KEY=" + client.getCookieValue();
        return lwssoValue;
    }

}
