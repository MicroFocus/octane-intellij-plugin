package com.hpe.adm.octane.services.exception;

public class ForbiddenServiceException extends ServiceException {

    public ForbiddenServiceException(String message, Throwable cause) {
        super(message, cause);
    }

    public ForbiddenServiceException(Throwable cause) {
        super(cause);
    }

}
