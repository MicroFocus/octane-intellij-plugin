package com.hpe.adm.octane.services.nonentity;

import com.google.gson.JsonParser;
import com.hpe.adm.nga.sdk.network.OctaneHttpRequest;
import com.hpe.adm.nga.sdk.network.OctaneHttpResponse;
import com.hpe.adm.nga.sdk.network.google.GoogleHttpClient;
import com.hpe.adm.octane.services.connection.ConnectionSettings;

import static com.hpe.adm.octane.services.util.ClientType.HPE_MQM_UI;


public class DownloadScriptService extends AuthenticationService {

    public String getGherkinTestScriptContent(long testId) {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

        if (authenticate()) {
                OctaneHttpRequest request = new OctaneHttpRequest.GetOctaneHttpRequest(connectionSettings.getBaseUrl() + "/api/shared_spaces/" + connectionSettings.getSharedSpaceId() +
                        "/workspaces/" + connectionSettings.getWorkspaceId() + "/tests/" + testId + "/script");
                OctaneHttpResponse response = new GoogleHttpClient(connectionSettings.getBaseUrl(),HPE_MQM_UI.name()).execute(request);
                String jsonString = response.getContent();

            return new JsonParser().parse(jsonString).getAsJsonObject().get("script").getAsString();
        }
        return null;
    }
}
