package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.EntityUtil;

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

        Map<Entity, Query.QueryBuilder> filterCriteria = new HashMap<>();

        //Standard backlog items
        filterCriteria.put(GHERKIN_TEST,
                    createPhaseQuery(TEST, "new", "indesign")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(MANUAL_TEST,
                    createPhaseQuery(TEST, "new", "indesign")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(DEFECT,
                    createPhaseQuery(DEFECT, "new", "inprogress", "intesting")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(USER_STORY,
                    createPhaseQuery(USER_STORY, "new", "inprogress", "intesting")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(TASK,
                    createPhaseQuery(TASK, "new", "inprogress")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(QUALITY_STORY,
                    createPhaseQuery(QUALITY_STORY, "new", "inprogress")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(QUALITY_STORY,
                    createPhaseQuery(QUALITY_STORY, "new", "inprogress")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(QUALITY_STORY,
                    createPhaseQuery(QUALITY_STORY, "new", "inprogress")
                .and(createCurrentUserQuery("owner"))
        );

        filterCriteria.put(MANUAL_TEST_RUN,
                    createNativeStatusQuery("list_node.run_native_status.blocked", "list_node.run_native_status.not_completed")
                .and(createCurrentUserQuery("run_by"))
                .and(new Query.QueryBuilder("parent_suite", Query::equalTo, null))
        );

        filterCriteria.put(TEST_SUITE_RUN,
                createNativeStatusQuery("list_node.run_native_status.blocked", "list_node.run_native_status.not_completed")
                        .and(createCurrentUserQuery("run_by"))
                        .and(new Query.QueryBuilder("parent_suite", Query::equalTo, null))
        );

        filterCriteria.put(COMMENT, createCurrentUserQuery("mention_user"));

        Collection<EntityModel> result = new ArrayList<>();

        //TODO, known subtypes should be under same rest call
        filterCriteria
                .keySet()
                .stream()
                .flatMap(entityType -> entityService.findEntities(entityType, filterCriteria.get(entityType), fieldListMap.get(entityType)).stream())
                .filter(entityModel -> !EntityUtil.containsEntityModel(result, entityModel))
                .forEach(result::add);

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
    private Query.QueryBuilder createNativeStatusQuery(String... logicalNames) {
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

    private Query.QueryBuilder createCurrentUserQuery(String fieldName){
        return new Query.QueryBuilder(fieldName, Query::equalTo,
                new Query.QueryBuilder("id", Query::equalTo, userService.getCurrentUserId()));
    }

}
