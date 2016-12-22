package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.integrationtests.IntegrationTestBase;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.junit.Assert;
import org.junit.Test;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.DEFECT;
import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.USER_STORY;

public class EntityServiceITCase extends IntegrationTestBase{

    @Inject
    private EntityService entityService;

    @Inject
    private TestService testService;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    @Test
    public void testEntityConstants() {

        Collection<EntityModel> entities;
        entities = entityService.findEntities(Entity.MANUAL_TEST);

        if(entities.size()>0){
            EntityModel firstEntity = entities.iterator().next();
            Assert.assertEquals(Entity.getEntityType(firstEntity), Entity.MANUAL_TEST);
        }

        entities = entityService.findEntities(DEFECT);
        if(entities.size()>0){
            EntityModel firstEntity = entities.iterator().next();
            Assert.assertEquals(Entity.getEntityType(firstEntity), DEFECT);
        }

        entities = entityService.findEntities(USER_STORY);
        if(entities.size()>0){
            EntityModel firstEntity = entities.iterator().next();
            Assert.assertEquals(Entity.getEntityType(firstEntity), USER_STORY);
        }
    }

    @Test
    public void testGetMyWork(){
        Collection<EntityModel> entityModels = entityService.getMyWork();
        System.out.println(entityModels.size());
    }

    @Test
    @Inject
    public void testConnection(){
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

        try{
            testService.getOctane(connectionSettings);
        } catch (Exception e){
            Assert.fail(e.toString());
        }

        try{
            testService.getOctane(connectionSettings);
        } catch (Exception e){
            Assert.fail(e.toString());
        }

    }

    @Test
    @Inject
    public void testPhases() {
        Collection<EntityModel> phases = entityService.findEntities(Entity.PHASE);
    }

    @Test
    @Inject
    public void testTransition() {
        Collection<EntityModel> transitions = entityService.findEntities(Entity.TRANSITION);
    }

    @Test
    @Inject
    public void testNextPhase() {
        //entityType
        Long currentPhaseId = 1002L;

        Set<String> fields = new HashSet<>();
        fields.add("source_phase");
        fields.add("target_phase");
        fields.add("is_primary");
        fields.add("entity");
        Collection<EntityModel>
                transitions = entityService.findEntities(Entity.TRANSITION, new Query.QueryBuilder("entity", Query::equalTo, DEFECT.getSubtypeName()), fields);


        for (EntityModel transition : transitions) {
            Long tempPhase = Long.valueOf(UiUtil.getUiDataFromModel(transition.getValue("source_phase"), "id"));
            if (currentPhaseId.equals(tempPhase)) {
                System.out.println(UiUtil.getUiDataFromModel(transition.getValue("target_phase"), "name"));
                System.out.println(UiUtil.getUiDataFromModel(transition.getValue("target_phase"), "is_primary"));
            }
        }
    }

}
