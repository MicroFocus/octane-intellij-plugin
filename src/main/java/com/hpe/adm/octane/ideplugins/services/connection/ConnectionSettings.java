package com.hpe.adm.octane.ideplugins.services.connection;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ConnectionSettings that = (ConnectionSettings) o;

        if (baseUrl != null ? !baseUrl.equals(that.baseUrl) : that.baseUrl != null) return false;
        if (sharedSpaceId != null ? !sharedSpaceId.equals(that.sharedSpaceId) : that.sharedSpaceId != null)
            return false;
        if (workspaceId != null ? !workspaceId.equals(that.workspaceId) : that.workspaceId != null) return false;
        if (userName != null ? !userName.equals(that.userName) : that.userName != null) return false;
        return password != null ? password.equals(that.password) : that.password == null;

    }

    @Override
    public int hashCode() {
        int result = baseUrl != null ? baseUrl.hashCode() : 0;
        result = 31 * result + (sharedSpaceId != null ? sharedSpaceId.hashCode() : 0);
        result = 31 * result + (workspaceId != null ? workspaceId.hashCode() : 0);
        result = 31 * result + (userName != null ? userName.hashCode() : 0);
        result = 31 * result + (password != null ? password.hashCode() : 0);
        return result;
    }

    /**
     * Set the internal state of the object to math the param
     * @param connectionSettings
     */
    public void setState(ConnectionSettings connectionSettings){
        setBaseUrl(connectionSettings.getBaseUrl());
        setPassword(connectionSettings.getPassword());
        setUserName(connectionSettings.getUserName());
        setSharedSpaceId(connectionSettings.getSharedSpaceId());
        setWorkspaceId(connectionSettings.getWorkspaceId());
    }

}
