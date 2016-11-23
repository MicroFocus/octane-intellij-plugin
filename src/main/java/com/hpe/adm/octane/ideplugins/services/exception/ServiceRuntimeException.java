package com.hpe.adm.octane.ideplugins.services.exception;

public class ServiceRuntimeException extends RuntimeException {

    public ServiceRuntimeException(String message, Throwable cause) {
        super(message, cause);
    }

    public ServiceRuntimeException(String message) {
        super(message);
    }

    public ServiceRuntimeException(Throwable cause) {
        super(cause);
    }

}
