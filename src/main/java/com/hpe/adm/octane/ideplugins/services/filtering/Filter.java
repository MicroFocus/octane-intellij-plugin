package com.hpe.adm.octane.ideplugins.services.filtering;

import com.hpe.adm.nga.sdk.Query;
import com.hpe.adm.nga.sdk.model.FieldModel;

/**
 * These object is intended to be used in the gui and persisted for future use
 * @param <T>
 */
public class Filter<T> {

    private Entity entity;
    private FieldModel<T> fieldModel;
    private Comparator comparator;

    public Filter(Entity entity, FieldModel<T> fieldModel, Comparator comparator) {
        this.entity = entity;
        this.fieldModel = fieldModel;
        this.comparator = comparator;
    }

    public Entity getEntity() {
        return entity;
    }

    public FieldModel<T> getFieldModel() {
        return fieldModel;
    }

    public Comparator getComparator() {
        return comparator;
    }

    /**
     * Create a query builder for the sdk from this Filter object
     * @return
     */
    public Query.QueryBuilder createQueryBuilder(){
        Query.QueryBuilder builder;
        builder = new Query.QueryBuilder(getFieldModel().getName(), getComparator().getFunction(), getFieldModel().getValue());
        return builder;
    }

}
