package com.hpe.adm.octane.services.nonentity;

import com.google.gson.JsonParser;
import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.network.OctaneHttpClient;
import com.hpe.adm.nga.sdk.network.OctaneHttpRequest;
import com.hpe.adm.nga.sdk.network.OctaneHttpResponse;
import com.hpe.adm.octane.services.connection.ConnectionSettings;
import com.hpe.adm.octane.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.services.connection.HttpClientProvider;


public class DownloadScriptService {
    @Inject
    protected HttpClientProvider httpClientProvider;
    @Inject
    protected ConnectionSettingsProvider connectionSettingsProvider;

    public String getGherkinTestScriptContent(long testId) {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        OctaneHttpClient httpClient = httpClientProvider.geOctaneHttpClient();
        if (null != httpClient) {
            OctaneHttpRequest request = new OctaneHttpRequest.GetOctaneHttpRequest(connectionSettings.getBaseUrl() + "/api/shared_spaces/" + connectionSettings.getSharedSpaceId() +
                    "/workspaces/" + connectionSettings.getWorkspaceId() + "/tests/" + testId + "/script");
            OctaneHttpResponse response = httpClient.execute(request);
            String jsonString = response.getContent();

            return new JsonParser().parse(jsonString).getAsJsonObject().get("script").getAsString();
        }
        return null;
    }
}
