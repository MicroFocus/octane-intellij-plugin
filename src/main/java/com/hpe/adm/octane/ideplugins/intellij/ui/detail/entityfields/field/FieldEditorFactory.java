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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;


import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.nga.sdk.query.Query;
import com.hpe.adm.nga.sdk.query.QueryMethod;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.EntityComboBox;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.MetadataService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

public class FieldEditorFactory {

    private static final int COMBO_BOX_ENTITY_LIMIT = 100;

    @Inject
    private MetadataService metadataService;

    @Inject
    private EntityService entityService;

    public FieldEditorFactory() {

    }

    public FieldEditor createFieldEditor(EntityModelWrapper entityModelWrapper, String fieldName) {
        Entity entityType = entityModelWrapper.getEntityType();
        FieldMetadata fieldMetadata = metadataService.getMetadata(entityType, fieldName);

        FieldEditor fieldEditor = null;

        //EntityFieldsConstants.FIELD_APPMODULE is a wannabe tree, need to make special ui for it
        if (!fieldMetadata.isEditable() ||
                fieldMetadata.isFinal() ||
                fieldName.equals(DetailsViewDefaultFields.FIELD_APPMODULE) ||
                fieldName.equals(DetailsViewDefaultFields.FIELD_ENVIROMENT)) {
            fieldEditor = new ReadOnlyFieldEditor();
        } else {
            switch (fieldMetadata.getFieldType()) {
                case Integer:
                    fieldEditor = new NumericFieldEditor(false);
                    break;
                case Float:
                    fieldEditor = new NumericFieldEditor(true);
                    break;
                case String:
                    fieldEditor = new StringFieldEditor();
                    break;
                case Boolean:
                    fieldEditor = new BooleanFieldEditor();
                    break;
                case DateTime:
                    fieldEditor = new DateTimeFieldEditor();
                    break;
                case Reference:
                    try {
                        fieldEditor = createReferenceFieldEditor(entityModelWrapper, fieldMetadata);
                    } catch (Exception e) {
                        fieldEditor = new ReadOnlyFieldEditor();
                    }
                    break;
                default:
                    fieldEditor = new ReadOnlyFieldEditor();
                    break;
            }
        }

        try {
            fieldEditor.setField(entityModelWrapper, fieldName);
        } catch (Exception ex) {
            fieldEditor = new ReadOnlyFieldEditor();
            fieldEditor.setField(entityModelWrapper, fieldName);
        }
        return fieldEditor;
    }

    private ReferenceFieldEditor createReferenceFieldEditor(EntityModelWrapper entityModelWrapper, FieldMetadata fieldMetadata) {
        FieldMetadata.Target[] targets = fieldMetadata.getFieldTypedata().getTargets();
        if (targets.length != 1) {
            throw new RuntimeException("Multiple target refrence fields not supported, fieldname: " + fieldMetadata.getName());
        }

        FieldMetadata.Target target = targets[0];
        String logicalName = target.logicalName();
        Entity targetEntity = getEntityType(target.getType());

        // List node loader
        EntityComboBox.EntityLoader entityLoader;

        if (Entity.LIST_NODE == targetEntity) {
            entityLoader = createListNodeEntityLoader(logicalName);
        } else if (targetEntity != null) { //known entity, other than LIST_NODE
            entityLoader = createGenericEntityLoader(getEntityType(target.getType()), entityModelWrapper);
        } else {
            throw new RuntimeException("Refrence entity type not supported: " + target.getType() + ", fieldname: " + fieldMetadata.getName());
        }

        ReferenceFieldEditor fieldEditor = new ReferenceFieldEditor();

        if (fieldMetadata.getFieldTypedata().isMultiple()) {
            fieldEditor.setMultiSelect(true);
        } else {
            fieldEditor.setMultiSelect(false);
        }

        fieldEditor.setEntityLoader(entityLoader);
        return fieldEditor;
    }

    private EntityComboBox.EntityLoader createListNodeEntityLoader(String targetLogicalName) {
        return (searchQuery) -> {
            Query.QueryBuilder qb = Query.statement("list_root", QueryMethod.EqualTo,
                    Query.statement("logical_name", QueryMethod.EqualTo, targetLogicalName));

            Collection<EntityModel> entities = entityService.findEntities(Entity.LIST_NODE, qb, null);

            //for some reason list nodes are not server side filterable, so you have to do it client side
            if (!searchQuery.isEmpty()) {
                String sanitizedSearchQuery = searchQuery.trim().toLowerCase();

                entities =
                        entities
                                .stream()
                                .filter(entityModel -> {
                                    String listNodeName = Util.getUiDataFromModel(entityModel.getValue("name"));
                                    listNodeName = listNodeName.trim();
                                    listNodeName = listNodeName.toLowerCase();
                                    return stringLike(listNodeName, sanitizedSearchQuery);
                                })
                                .collect(Collectors.toList());
            }
            return entities;
        };
    }

    private EntityComboBox.EntityLoader createGenericEntityLoader(Entity entity, EntityModelWrapper entityModelWrapper) {
        return (searchQuery) -> {
            Query.QueryBuilder qb = null;

            if (!searchQuery.isEmpty()) {
                String labelFieldName;
                if (entity == Entity.WORKSPACE_USER) {
                    labelFieldName = DetailsViewDefaultFields.FIELD_FULL_NAME;
                } else {
                    labelFieldName = DetailsViewDefaultFields.FIELD_NAME;
                }

                qb = Query.statement(labelFieldName,
                        QueryMethod.EqualTo,
                        "*" + searchQuery + "*");
            }

            //Restrict sprint dropdown to current release, if there's no current release, display no
            if (Entity.SPRINT == entity) {
                if (entityModelWrapper.hasValue(DetailsViewDefaultFields.FIELD_RELEASE)) {
                    ReferenceFieldModel releaseFieldModel = (ReferenceFieldModel) entityModelWrapper.getValue(DetailsViewDefaultFields.FIELD_RELEASE);
                    String releaseId = releaseFieldModel.getValue().getId();

                    Query.QueryBuilder releaseQb =
                            Query.statement(DetailsViewDefaultFields.FIELD_RELEASE, QueryMethod.EqualTo,
                                    Query.statement(DetailsViewDefaultFields.FIELD_ID, QueryMethod.EqualTo, releaseId));

                    //join the two query builders
                    qb = qb != null ? qb.and(releaseQb) : releaseQb;
                } else {
                    return Collections.emptyList();
                }
            }

            return entityService.findEntities(entity, qb, null, null, null, COMBO_BOX_ENTITY_LIMIT);
        };
    }

    private static boolean stringLike(String str, String expr) {
        if (str == null || expr == null) {
            return false;
        }
        return str.contains(expr) || expr.contains(str);
    }

    private Entity getEntityType(String type) {
        return Arrays.stream(Entity.values())
                .filter(entity -> entity.getEntityName().equals(type))
                .findAny()
                .orElse(null);
    }
}
