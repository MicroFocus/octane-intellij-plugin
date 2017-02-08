package com.hpe.adm.octane.ideplugins.services.util;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.apache.commons.lang.ObjectUtils;

import java.util.Collection;

public class EntityUtil {

    public static boolean areEqual(EntityModel em1, EntityModel em2){
        if(!ObjectUtils.equals(Entity.getEntityType(em1), Entity.getEntityType(em2))){
            return false;
        }

        String em1Id = "";
        if(em1.getValue("id") != null){
            em1Id = em1.getValue("id").getValue().toString();
        }

        String em2Id = "";
        if(em2.getValue("id") != null){
            em2Id = em1.getValue("id").getValue().toString();
        }

        if(!em1Id.equals(em2Id)){
            return false;
        }

        return true;
    }

    public static boolean containsEntityModel(Collection<EntityModel> collection, EntityModel entityModel){
        for(EntityModel em : collection){
            if(areEqual(em, entityModel)){
                return true;
            }
        }
        return false;
    }

}