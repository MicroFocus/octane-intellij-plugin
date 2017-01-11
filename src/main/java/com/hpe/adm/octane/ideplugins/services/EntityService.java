package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.EntityList;
import com.hpe.adm.nga.sdk.EntityListService;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.connection.OctaneProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.SdkUtil;

import java.util.*;

import static com.hpe.adm.nga.sdk.utils.CommonUtils.getIdFromEntityModel;
import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.*;


public class EntityService {

    @Inject
    private UserService userService;

    @Inject
    private OctaneProvider octaneProvider;


    public Collection<EntityModel> getMyWork() {
        return getMyWork(new HashMap<>());
    }

    /**
     * Can specify which fields to fetch for which entity
     *
     * @param fieldListMap if there is no entry for an entity type all fields are fetched
     * @return
     */
    public Collection<EntityModel> getMyWork(Map<Entity, Set<String>> fieldListMap) {

        Map<Entity, Query.QueryBuilder> myWorkFilter = new HashMap<>();

        myWorkFilter.put(GHERKIN_TEST, createPhaseQuery(TEST, "new", "indesign"));
        myWorkFilter.put(MANUAL_TEST, createPhaseQuery(TEST, "new", "indesign"));
        myWorkFilter.put(DEFECT, createPhaseQuery(DEFECT, "new", "inprogress", "intesting"));
        myWorkFilter.put(USER_STORY, createPhaseQuery(USER_STORY, "new", "inprogress", "intesting"));
        myWorkFilter.put(TASK, createPhaseQuery(TASK, "new", "inprogress"));

        Query.QueryBuilder currentUserQuery = new Query.QueryBuilder("owner", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, userService.getCurrentUserId()));

        Collection<EntityModel> result = new ArrayList<>();

        //TODO, known subtypes should be under same rest call
        myWorkFilter.keySet().forEach(entityType -> {
                    Query.QueryBuilder query = myWorkFilter.get(entityType).and(currentUserQuery);
                    result.addAll(findEntities(entityType, query, fieldListMap.get(entityType)));
                }
        );
        Query.QueryBuilder parentIsNull = new Query.QueryBuilder("parent_suite", Query::equalTo, null);
        Query.QueryBuilder runByQuery = new Query.QueryBuilder("run_by", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, userService.getCurrentUserId()));
        Query.QueryBuilder nativeStatusQuery = createNativeStatusQuery("list_node.run_native_status.blocked", "list_node.run_native_status.not_completed");
        Query.QueryBuilder finalQuery = nativeStatusQuery.and(runByQuery).and(parentIsNull);
        result.addAll(findEntities(MANUAL_TEST_RUN, finalQuery, fieldListMap.get(MANUAL_TEST_RUN)));
        result.addAll(findEntities(TEST_SUITE_RUN, finalQuery, fieldListMap.get(TEST_SUITE_RUN)));

        Query.QueryBuilder mentionedUserQuery = new Query.QueryBuilder("mention_user", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, userService.getCurrentUserId()));

        Collection<EntityModel> comments = findEntities(Entity.COMMENT, mentionedUserQuery, fieldListMap.get(Entity.COMMENT));
        result.addAll(comments);

        return result;
    }

    /**
     * Constructs a metaphase query builder to match "logical_name":"metaphase.entity.phasename",
     *
     * @param entity
     * @param phases
     * @return
     */
    private Query.QueryBuilder createPhaseQuery(Entity entity, String... phases) {
        Query.QueryBuilder phaseQueryBuilder = null;
        for (String phaseName : phases) {
            String phaseLogicalName = "metaphase." + entity.getTypeName() + "." + phaseName;
            Query.QueryBuilder currentPhaseQueryBuilder =
                    new Query.QueryBuilder("metaphase", Query::equalTo,
                            new Query.QueryBuilder("logical_name", Query::equalTo, phaseLogicalName)
                    );
            if (phaseQueryBuilder == null) {
                phaseQueryBuilder = currentPhaseQueryBuilder;
            } else {
                phaseQueryBuilder = phaseQueryBuilder.or(currentPhaseQueryBuilder);
            }
        }

        return new Query.QueryBuilder("phase", Query::equalTo, phaseQueryBuilder);
    }
    /**
     *
     * @param logicalNames
     * @return
     */
    public Query.QueryBuilder createNativeStatusQuery(String... logicalNames) {
        Query.QueryBuilder nativeStatusQueryBuilder = null;
        for (String logicalName : logicalNames) {
            Query.QueryBuilder currentNativeStatusQueryBuilder =
                    new Query.QueryBuilder("logical_name", Query::equalTo, logicalName);
            if (nativeStatusQueryBuilder == null) {
                nativeStatusQueryBuilder = currentNativeStatusQueryBuilder;
            } else {
                nativeStatusQueryBuilder = nativeStatusQueryBuilder.or(currentNativeStatusQueryBuilder);
            }
        }
        return new Query.QueryBuilder("native_status", Query::equalTo, nativeStatusQueryBuilder);
    }

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

        return getRequest.execute();
    }

    /**
     * @param entityType
     * @param entityId
     * @return
     * @throws ServiceException
     */
    public EntityModel findEntity(Entity entityType, Long entityId) throws ServiceException {
        EntityModel result;
        try {
            result = octaneProvider.getOctane().entityList(entityType.getApiEntityName()).at(entityId.intValue()).get().execute();
        } catch (Exception e) {
            String message = "Failed to get " + entityType.name() + ": " + entityId;
            if (e instanceof OctaneException) {
                message = message + "<br>" + SdkUtil.getMessageFromOctaneException((OctaneException) e);
            }
            throw new ServiceException(message, e);
        }

        return result;
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
            Long tempPhase = Long.valueOf(UiUtil.getUiDataFromModel(transition.getValue("source_phase"), "id"));
            if (currentPhaseId.equals(tempPhase)) {
                possibleTransitions.add(transition);
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
}
