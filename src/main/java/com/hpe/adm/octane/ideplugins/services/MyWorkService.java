package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import java.util.*;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.*;
import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.MANUAL_TEST_RUN;
import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.TEST_SUITE_RUN;

public class MyWorkService {

    @Inject
    EntityService entityService;

    @Inject
    UserService userService;

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
        myWorkFilter.put(QUALITY_STORY, createPhaseQuery(QUALITY_STORY, "new", "inprogress"));

        Query.QueryBuilder currentUserQuery = new Query.QueryBuilder("owner", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, userService.getCurrentUserId()));

        Collection<EntityModel> result = new ArrayList<>();

        //TODO, known subtypes should be under same rest call
        myWorkFilter.keySet().forEach(entityType -> {
                    Query.QueryBuilder query = myWorkFilter.get(entityType).and(currentUserQuery);
                    result.addAll(entityService.findEntities(entityType, query, fieldListMap.get(entityType)));
                }
        );

        //Fetching manual tests and test suites
        Query.QueryBuilder parentIsNull = new Query.QueryBuilder("parent_suite", Query::equalTo, null);
        Query.QueryBuilder runByQuery = new Query.QueryBuilder("run_by", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, userService.getCurrentUserId()));
        Query.QueryBuilder nativeStatusQuery = createNativeStatusQuery("list_node.run_native_status.blocked", "list_node.run_native_status.not_completed");
        Query.QueryBuilder finalQuery = nativeStatusQuery.and(runByQuery).and(parentIsNull);
        result.addAll(entityService.findEntities(MANUAL_TEST_RUN, finalQuery, fieldListMap.get(MANUAL_TEST_RUN)));
        result.addAll(entityService.findEntities(TEST_SUITE_RUN, finalQuery, fieldListMap.get(TEST_SUITE_RUN)));

        //Fetching comments
        Query.QueryBuilder mentionedUserQuery = new Query.QueryBuilder("mention_user", Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, userService.getCurrentUserId()));

        Collection<EntityModel> comments = entityService.findEntities(Entity.COMMENT, mentionedUserQuery, fieldListMap.get(Entity.COMMENT));
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

}
