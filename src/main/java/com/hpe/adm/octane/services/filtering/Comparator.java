package com.hpe.adm.octane.services.filtering;

import com.hpe.adm.nga.sdk.Query;

import java.util.function.BiFunction;

/**
 * Constants for skd query builder functions
 */
public enum Comparator{
    EQ(Query::equalTo),
    LT(Query::lessThan),
    GT(Query::greaterThan),
    EL(Query::greaterThanOrEqualTo),
    GE(Query::greaterThanOrEqualTo);

    private BiFunction<String, Object, String> function;

    Comparator(BiFunction<String, Object, String> function){
        this.function = function;
    }

    public BiFunction<String, Object, String> getFunction() {
        return function;
    }
}