package com.hpe.adm.octane.ideplugins.services;

public class ConnectionSettings {

    private String baseUrl;
    private Long sharedSpaceId;
    private Long workspaceId;
    private String userName;
    private String password;
    //Singleton
    private static ConnectionSettings connectionSettings;

    public ConnectionSettings(String baseUrl, Long sharedSpaceId, Long workspaceId, String userName, String password) {
        this.baseUrl = baseUrl;
        this.sharedSpaceId = sharedSpaceId;
        this.workspaceId = workspaceId;
        this.userName = userName;
        this.password = password;
    }

    public static ConnectionSettings init(String baseUrl, Long sharedSpaceId, Long workspaceId, String userName, String password) {
        if(isInit()){
            throw new RuntimeException("Already init");
        }
        connectionSettings = new ConnectionSettings(baseUrl, sharedSpaceId, workspaceId, userName,password);
        return connectionSettings;
    }

    public static boolean isInit() {
        return connectionSettings != null;
    }

    public static ConnectionSettings getInstance(){
        return connectionSettings;
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    public Long getSharedSpaceId() {
        return sharedSpaceId;
    }

    public Long getWorkspaceId() {
        return workspaceId;
    }

    public String getUserName() {
        return userName;
    }

    public String getPassword() {
        return password;
    }
}
