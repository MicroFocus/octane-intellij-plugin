package com.hpe.adm.octane.ideplugins.services;

public class ConnectionSettings {

    private String baseUrl;
    private Long sharedSpaceId;
    private Long workspaceId;
    private String userName;
    private String password;

    public ConnectionSettings() {
    }
    public ConnectionSettings(String baseUrl, Long sharedSpaceId, Long workspaceId, String userName, String password) {
        this.baseUrl = baseUrl;
        this.sharedSpaceId = sharedSpaceId;
        this.workspaceId = workspaceId;
        this.userName = userName;
        this.password = password;
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    public Long getSharedSpaceId() {
        return sharedSpaceId;
    }

    public void setSharedSpaceId(Long sharedSpaceId) {
        this.sharedSpaceId = sharedSpaceId;
    }

    public Long getWorkspaceId() {
        return workspaceId;
    }

    public void setWorkspaceId(Long workspaceId) {
        this.workspaceId = workspaceId;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}
