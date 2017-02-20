package com.hpe.adm.octane.services.util;


public enum AccessLevelValue {
    /*API which is publicly available*/
    PUBLIC(1),
    /* internal but Backward compatible*/
    PUBLIC_INTERNAL(2),
    /* Non backward compatible */
    PROTECTED(3);

    // strengthFactor - If X is stronger than Y then X is more strict and has higher strengthFactor
    // For instance Protected(3) is stronger than Public_Internal(2). Public_Internal(2) is stronger than Public(1)
    // SO PUBLIC < PUBLIC_INTERNAL < PROTECTED
    // WEAKEST …....…………………………STRONGEST
    // That is why method access level can not be weaker than the resource access level
    private final int strengthFactor;

    AccessLevelValue(int strengthFactor){
        this.strengthFactor = strengthFactor;
    }

    public boolean strongerThan(AccessLevelValue otherAccessLevel) {
        return this.strengthFactor > otherAccessLevel.strengthFactor;
    }

    public boolean weakerThan(AccessLevelValue otherAccessLevel) {
        return this.strengthFactor < otherAccessLevel.strengthFactor;
    }

    public boolean strongerThanOrEqualTo(AccessLevelValue otherAccessLevel) {
        return this == otherAccessLevel || strongerThan(otherAccessLevel);
    }
}