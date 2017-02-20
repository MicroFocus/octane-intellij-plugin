package com.hpe.adm.octane.services.nonentity;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.authentication.Authentication;
import com.hpe.adm.nga.sdk.authentication.SimpleUserAuthentication;
import com.hpe.adm.nga.sdk.network.OctaneHttpRequest;
import com.hpe.adm.nga.sdk.network.OctaneHttpResponse;
import com.hpe.adm.nga.sdk.network.google.GoogleHttpClient;
import com.hpe.adm.octane.services.UserService;
import com.hpe.adm.octane.services.connection.ConnectionSettings;

import static com.hpe.adm.octane.services.util.ClientType.HPE_MQM_UI;

public class SharedSpaceLevelRequestService extends AuthenticationService {

    @Inject
    private UserService userService;

    public String getCurrentWorkspaceName() {

        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        String retVal = " ";
        Authentication auth = new SimpleUserAuthentication(connectionSettings.getUserName(), connectionSettings.getPassword());
        GoogleHttpClient httpClient = new GoogleHttpClient(connectionSettings.getBaseUrl(), HPE_MQM_UI.name());

        OctaneHttpRequest request = new OctaneHttpRequest.GetOctaneHttpRequest(connectionSettings.getBaseUrl() + "/api/shared_spaces/" +
                connectionSettings.getSharedSpaceId() + "/workspaces" + "?fields = id,name" + "&query = \"users={id=" + userService.getCurrentUserId() + "}\"");

        OctaneHttpResponse response = httpClient.execute(request);
        String jsonString = response.getContent();
        JsonArray dataArray = new JsonParser().parse(jsonString).getAsJsonObject().get("data").getAsJsonArray();
        for (JsonElement elem : dataArray) {
            String id = elem.getAsJsonObject().get("id").getAsString();
            if (Long.valueOf(id).equals(connectionSettings.getWorkspaceId())) {
                retVal = elem.getAsJsonObject().get("name").getAsString();
            }
        }
        return retVal;
    }
}
