package com.hpe.adm.octane.ideplugins.integrationtests;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.MultiReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.UserService;
import com.hpe.adm.octane.ideplugins.services.connection.OctaneProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import hidden.edu.emory.mathcs.backport.java.util.Arrays;
import org.junit.Test;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class DummyTest extends IntegrationTestBase {

    @Inject
    private EntityService entityService;

    @Inject
    private UserService userService;

    @Inject
    private OctaneProvider octaneProvider;

    @Test
    public void testytestest() {

        EntityModel userStory = null;
        try {
            userStory = entityService.findEntity(Entity.USER_STORY, 1002L);
        } catch (ServiceException e) {
            e.printStackTrace();
            System.out.println("tough luck");
        }
        EntityModel currentUser = userService.getCurrentUser();

        MultiReferenceFieldModel referenceFieldModel1 = new MultiReferenceFieldModel("my_follow_items_owner", getSet(currentUser));
        MultiReferenceFieldModel referenceFieldModel2 = new MultiReferenceFieldModel("my_new_items_owner", getSet(currentUser));

        userStory.setValue(referenceFieldModel1);
        userStory.setValue(referenceFieldModel2);

        Octane octane = octaneProvider.getOctane();

        Integer id = Integer.valueOf(userStory.getValue("id").getValue().toString());
        EntityModel updateEntity = new EntityModel();
        updateEntity.setValue(referenceFieldModel1);
        updateEntity.setValue(referenceFieldModel2);

        try {
            octane.entityList(Entity.USER_STORY.getApiEntityName()).at(id).update().entity(updateEntity).execute();
        } catch (OctaneException ex){
            System.out.println(ex);
        }

    }

    @Test
    public void testytestest2() {

        EntityModel userStory = null;
        try {
            userStory = entityService.findEntity(Entity.USER_STORY, 1002L);
        } catch (ServiceException e) {
            e.printStackTrace();
            System.out.println("tough luck");
        }
        EntityModel currentUser = userService.getCurrentUser();

        MultiReferenceFieldModel referenceFieldModel1 = new MultiReferenceFieldModel("my_follow_items_owner", java.util.Collections.emptySet());
        MultiReferenceFieldModel referenceFieldModel2 = new MultiReferenceFieldModel("my_new_items_owner", java.util.Collections.emptySet());

        userStory.setValue(referenceFieldModel1);
        userStory.setValue(referenceFieldModel2);

        Octane octane = octaneProvider.getOctane();

        Integer id = Integer.valueOf(userStory.getValue("id").getValue().toString());
        EntityModel updateEntity = new EntityModel();
        updateEntity.setValue(referenceFieldModel1);
        updateEntity.setValue(referenceFieldModel2);

        try {
            octane.entityList(Entity.USER_STORY.getApiEntityName()).at(id).update().entity(updateEntity).execute();
        } catch (OctaneException ex){
            System.out.println(ex);
        }

    }

    @Test
    public void isSupported(){
        Octane octane = octaneProvider.getOctane();
        Collection<FieldMetadata> fields = octane.metadata().fields(Entity.USER_STORY.getTypeName()).execute();

        fields.forEach(fieldMetadata -> {
            if("my_follow_items_owner".equals(fieldMetadata.getName())){
                System.out.println("YES");
                return;
            }
        });
        System.out.println("NO");
    }


    private Set getSet(Object... fm) {
        Set set = new HashSet<>(Arrays.asList(fm));
        return set;
    }




}
