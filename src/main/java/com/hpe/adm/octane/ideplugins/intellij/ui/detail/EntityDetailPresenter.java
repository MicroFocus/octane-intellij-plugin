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

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.StringFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.actions.RefreshCurrentEntityAction;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.BusinessErrorReportingDialog;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions.SaveCurrentEntityAction;
import com.hpe.adm.octane.ideplugins.intellij.util.ExceptionHandler;
import com.hpe.adm.octane.ideplugins.intellij.util.HtmlTextEditor;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.MetadataService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.nonentity.ImageService;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.actionSystem.CustomShortcutSet;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.project.Project;
import org.json.JSONObject;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

public class EntityDetailPresenter implements Presenter<EntityDetailView> {

    private static final Logger logger = Logger.getInstance(EntityDetailPresenter.class);

    @Inject
    private Project project;
    @Inject
    private EntityService entityService;
    @Inject
    private MetadataService metadataService;
    @Inject
    private ImageService imageService;

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private Long entityId;
    private Entity entityType;
    private EntityModelWrapper entityModelWrapper;
    private Collection<FieldMetadata> fields;

    private EntityDetailView entityDetailView;

    private boolean stateChanged = false;

    public EntityDetailPresenter() {
    }

    public EntityDetailView getView() {
        return entityDetailView;
    }

    @Override
    @Inject
    public void setView(EntityDetailView entityDetailView) {
        this.entityDetailView = entityDetailView;

        Keymap keymap = KeymapManager.getInstance().getActiveKeymap();

        SaveCurrentEntityAction saveCurrentEntityAction = new SaveCurrentEntityAction();
        saveCurrentEntityAction.registerCustomShortcutSet(
                new CustomShortcutSet(keymap.getShortcuts(SaveCurrentEntityAction.class.getCanonicalName())),
                entityDetailView);
        entityDetailView.setSaveSelectedPhaseButton(saveCurrentEntityAction);

        RefreshCurrentEntityAction refreshCurrentEntityAction = new RefreshCurrentEntityAction();
        refreshCurrentEntityAction.registerCustomShortcutSet(
                new CustomShortcutSet(keymap.getShortcuts(RefreshCurrentEntityAction.class.getCanonicalName())),
                entityDetailView);
        entityDetailView.setRefreshEntityButton(refreshCurrentEntityAction);

        entityDetailView.setOpenInBrowserButton();
        entityDetailView.setupFieldsSelectButton();
        entityDetailView.setupCommentsButton();

    }

    private IdePluginPersistentState.SettingsChangedHandler fieldSettingsChangedHandler = new IdePluginPersistentState.SettingsChangedHandler() {
        @Override
        public void stateChanged(IdePluginPersistentState.Key key, JSONObject value) {
            if (key == IdePluginPersistentState.Key.SELECTED_FIELDS) {
                entityDetailView.redrawFields();
            }
        }
    };

    public void setEntity(Entity entityType, Long entityId) {
        this.entityType = entityType;
        this.entityId = entityId;

        RestUtil.runInBackground(
                () -> {
                    try {
                        fields = metadataService.getVisibleFields(entityType);

                        Set<String> requestedFields = fields.stream().map(FieldMetadata::getName).collect(Collectors.toSet());
                        requestedFields.add("client_lock_stamp");

                        EntityModel entityModel = entityService.findEntity(this.entityType, this.entityId, requestedFields);
                        downloadMemoFieldImages(entityModel);

                        entityModelWrapper = new EntityModelWrapper(entityModel);

                        //The subtype field is absolutely necessary, yet the server sometimes has weird ideas and doesn't return it
                        if (entityType.isSubtype()) {
                            entityModelWrapper.setValue(new StringFieldModel(DetailsViewDefaultFields.FIELD_SUBTYPE, entityType.getSubtypeName()));
                        }

                        return entityModelWrapper;
                    } catch (Exception ex) {
                        ExceptionHandler exceptionHandler = new ExceptionHandler(ex, project);
                        exceptionHandler.showErrorNotification();
                        entityDetailView.setErrorMessage(ex.getMessage());
                        return null;
                    }
                },
                (entityModelWrapper) -> {
                    if (entityModelWrapper != null) {
                        entityDetailView.setEntityModel(entityModelWrapper, fields);
                        entityModelWrapper.addFieldModelChangedHandler((e) -> stateChanged = true);
                    }
                },
                project,
                null,
                "Loading entity " + entityType.name() + ": " + entityId);

        idePluginPersistentState.addStateChangedHandler(fieldSettingsChangedHandler);
    }

    private void downloadMemoFieldImages(EntityModel entityModel) {
        entityModel
                .getValues()
                .stream()
                .filter(fieldModel -> isMemoField(entityModel, fieldModel.getName()))
                .forEach(fieldModel ->
                        entityModel.setValue(new StringFieldModel(fieldModel.getName(), downloadImagesInsideOfMemoField(fieldModel)))
                );
    }

    private String downloadImagesInsideOfMemoField(FieldModel fieldModel) {
        String fieldValue = Util.getUiDataFromModel(fieldModel);
        fieldValue = HtmlTextEditor.removeHtmlStructure(fieldValue);
        fieldValue = HtmlTextEditor.getColoredHTML(fieldValue);

        try {
            fieldValue = imageService.downloadPictures(fieldValue);
        } catch (Exception ex) {
            ExceptionHandler exceptionHandler = new ExceptionHandler(ex, project);
            exceptionHandler.showErrorNotification();
            entityDetailView.setErrorMessage(ex.getMessage());
        }

        return fieldValue;
    }

    private void removeAllMemoFields(EntityModel entityModel) {
        Set<String> memoFields = entityModel
                        .getValues()
                        .stream()
                        .filter(fieldModel -> isMemoField(entityModel, fieldModel.getName()))
                        .map(FieldModel::getName)
                        .collect(Collectors.toSet());

        memoFields.forEach(entityModel::removeValue);
    }

    private boolean isMemoField(EntityModel entityModel, String fieldName) {
        try {
            FieldMetadata fieldMetadata = metadataService.getMetadata(Entity.getEntityType(entityModel), fieldName);
            return fieldMetadata.getFieldType() == FieldMetadata.FieldType.Memo;
        } catch (Exception ex) {
            return false;
        }
    }

    @Override
    public void closing() {
        idePluginPersistentState.removeStateChangedHandler(fieldSettingsChangedHandler);
        if (fields != null) { // can be null if first metadata req fails, then the tab is closed
            fields.clear();
        }
    }

    public boolean wasEntityChanged() {
        return stateChanged;
    }

    public void refresh() {
        entityDetailView.doRefresh();
        setEntity(entityType, entityId);
        stateChanged = false;
    }

    public void saveEntity() {
        RestUtil.runInBackground(() -> {
            try {
                EntityModel entityModel = entityModelWrapper.getEntityModel();
                // because memo fields are currently read only, and we modify them for the images,
                // we need to make sure we don't save the changes to them,
                removeAllMemoFields(entityModel);

                entityService.updateEntity(entityModel);
                entityDetailView.doRefresh();

                setEntity(entityType, entityId);
                stateChanged = false;

            } catch (OctaneException ex) {
                BusinessErrorReportingDialog berDialog = new BusinessErrorReportingDialog(project, ex);
                berDialog.show();

                switch (berDialog.getExitCode()) {
                    case BusinessErrorReportingDialog.EXIT_CODE_OPEN_IN_BROWSER: {
                        entityService.openInBrowser(entityModelWrapper.getEntityModel());
                    }
                    case BusinessErrorReportingDialog.EXIT_CODE_REFRESH: {
                        entityDetailView.doRefresh();
                        setEntity(entityType, entityId);
                        stateChanged = false;
                        break;
                    }
                    case BusinessErrorReportingDialog.EXIT_CODE_BACK:
                    default:
                }
            }

        }, null, "Failed to save entity", "Saving entity");
    }

}