package com.hpe.adm.octane.ideplugins.services;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.Octane;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.connection.OctaneProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class MetadataService {

    @Inject
    private OctaneProvider octaneProvider;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    private Map<Entity, Collection<FieldMetadata>> cache;

    public boolean hasFields(Entity entityType, String... fieldNames){

        if(cache == null){
            cache = new ConcurrentHashMap<>();
            init();
        }

        Octane octane = octaneProvider.getOctane();

        Collection<FieldMetadata> fields;

        if(!cache.containsKey(entityType)){
            fields = octane.metadata().fields(entityType.getEntityName()).execute();
            cache.put(entityType, fields);
        } else {
            fields = cache.get(entityType);
        }

        List<String> responseFieldNames = fields.stream().map(FieldMetadata::getName).collect(Collectors.toList());

        return Arrays.stream(fieldNames)
                .allMatch(responseFieldNames::contains);
    }

    public void eagerInit(Entity... entities){
        if(cache == null){
            cache = new ConcurrentHashMap<>();
            init();
        }

        Octane octane = octaneProvider.getOctane();

        Arrays.stream(entities)
                .parallel()
                .forEach(entityType -> cache.put(entityType, octane.metadata().fields(entityType.getEntityName()).execute()));
    }

    private void init(){
        cache = new ConcurrentHashMap<>();
        connectionSettingsProvider.addChangeHandler(()-> cache.clear());
    }

}