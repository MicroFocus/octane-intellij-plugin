package com.hpe.adm.octane.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.EntityList;
import com.hpe.adm.nga.sdk.EntityListService;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.services.util.Util;
import com.hpe.adm.octane.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.services.connection.OctaneProvider;
import com.hpe.adm.octane.services.exception.ServiceException;
import com.hpe.adm.octane.services.filtering.Entity;
import com.hpe.adm.octane.services.util.SdkUtil;
import com.hpe.adm.octane.services.util.UrlParser;

import java.awt.*;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import static com.hpe.adm.nga.sdk.utils.CommonUtils.getIdFromEntityModel;
import static com.hpe.adm.octane.services.util.Util.getUiDataFromModel;


public class EntityService {

    @Inject
    private OctaneProvider octaneProvider;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    public Collection<EntityModel> findEntities(Entity entity) {
        return findEntities(entity, null, null);
    }

    public Collection<EntityModel> findEntities(Entity entity, Query.QueryBuilder query, Set<String> fields) {
        EntityList entityList = octaneProvider.getOctane().entityList(entity.getApiEntityName());

        Query.QueryBuilder queryBuilder = null;

        if (entity.isSubtype()) {
            queryBuilder = entity.createMatchSubtypeQueryBuilder();
        }

        if (query != null) {
            if (queryBuilder == null) {
                queryBuilder = query;
            } else {
                queryBuilder = queryBuilder.and(query);
            }
        }

        EntityListService.Get getRequest = entityList.get();
        if (queryBuilder != null) {
            getRequest = getRequest.query(queryBuilder.build());
        }
        if (fields != null && fields.size() != 0) {
            getRequest = getRequest.addFields(fields.toArray(new String[]{}));
        }
        getRequest.addOrderBy("id", true);

        Collection<EntityModel> col = getRequest.execute();
        return col;
    }

    public Collection<EntityModel> findEntities(String apiEntity, Query.QueryBuilder query, Set<String> fields) {
        EntityList entityList = octaneProvider.getOctane().entityList(apiEntity);

        EntityListService.Get getRequest = entityList.get();
        if (query != null) {
            getRequest = getRequest.query(query.build());
        }
        if (fields != null && fields.size() != 0) {
            getRequest = getRequest.addFields(fields.toArray(new String[]{}));
        }
        getRequest.addOrderBy("id", true);

        Collection<EntityModel> col = getRequest.execute();
        return col;
    }

    /**
     * @param entityType
     * @param entityId
     * @return
     * @throws ServiceException
     */
    public EntityModel findEntity(Entity entityType, Long entityId, Set<String> fields) throws ServiceException {
        try {
            EntityListService.Entities.Get get =
                    octaneProvider.getOctane()
                            .entityList(entityType.getApiEntityName())
                            .at(entityId.intValue())
                            .get();

            if(fields != null && fields.size() != 0){
                get = get.addFields(fields.toArray(new String[]{}));
            }

            return get.execute();

        } catch (Exception e) {
            String message = "Failed to get " + entityType.name() + ": " + entityId;
            if (e instanceof OctaneException) {
                message = message + "<br>" + SdkUtil.getMessageFromOctaneException((OctaneException) e);
            }
            throw new ServiceException(message, e);
        }
    }

    public EntityModel findEntity(Entity entityType, Long entityId) throws ServiceException {
        return findEntity(entityType, entityId, null);
    }

    /**
     * Get next possible phases for an entity
     *
     * @param entityType
     * @param currentPhaseId
     * @return
     */
    public Collection<EntityModel> findPossibleTransitionFromCurrentPhase(Entity entityType, Long currentPhaseId) {
        Set<String> fields = new HashSet<>();
        fields.add("source_phase");
        fields.add("target_phase");
        fields.add("is_primary");
        fields.add("entity");


        ArrayList<EntityModel> possibleTransitions = new ArrayList<>();
        String entityName;
        if (entityType.isSubtype()) {
            entityName = entityType.getSubtypeName();
        } else {
            entityName = entityType.getTypeName();
        }
        Collection<EntityModel>
                transitions = findEntities(Entity.TRANSITION,
                new Query.QueryBuilder("entity", Query::equalTo, entityName), fields);

        for (EntityModel transition : transitions) {
            Long tempPhase = Long.valueOf(Util.getUiDataFromModel(transition.getValue("source_phase"), "id"));
            if (currentPhaseId.equals(tempPhase)) {
                if(transition.getValue("is_primary").getValue().equals(Boolean.TRUE)){
                    possibleTransitions.add(0, transition);
                } else {
                    possibleTransitions.add(transition);
                }
            }
        }
        return possibleTransitions;
    }

    public void updateEntityPhase(EntityModel entityModel, ReferenceFieldModel nextPhase) {
        int entityId = getIdFromEntityModel(entityModel);
        Entity entityType = Entity.getEntityType(entityModel);
        EntityList entityList = octaneProvider.getOctane().entityList(entityType.getApiEntityName());

        ReferenceFieldModel updatePhaseModel = new ReferenceFieldModel("phase", nextPhase.getValue());

        Set<FieldModel> fields = new HashSet<>();
        fields.add(updatePhaseModel);
        EntityModel updatedEntity = new EntityModel(fields);

        entityList.at(entityId).update().entity(updatedEntity).execute();
    }

    public void openInBrowser(EntityModel entityModel) {
        Entity entityType = Entity.getEntityType(entityModel);
        Integer entityId = Integer.valueOf(getUiDataFromModel(entityModel.getValue("id")));
        Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
        if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {
            try {
                Entity ownerEntityType = null;
                Integer ownerEntityId = null;
                if (entityType == Entity.COMMENT) {
                    ReferenceFieldModel owner = (ReferenceFieldModel) Util.
                            getContainerItemForCommentModel(entityModel);
                    ownerEntityType = Entity.getEntityType(owner.getValue());
                    ownerEntityId = Integer.valueOf(Util.getUiDataFromModel(owner, "id"));
                }
                URI uri =
                        UrlParser.createEntityWebURI(
                                connectionSettingsProvider.getConnectionSettings(),
                                entityType == Entity.COMMENT ? ownerEntityType : entityType,
                                entityType == Entity.COMMENT ? ownerEntityId : entityId);
                desktop.browse(uri);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

}
