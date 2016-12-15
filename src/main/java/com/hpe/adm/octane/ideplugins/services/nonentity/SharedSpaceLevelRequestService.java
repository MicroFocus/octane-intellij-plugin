package com.hpe.adm.octane.ideplugins.services.nonentity;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.network.HttpClient;
import com.hpe.adm.nga.sdk.network.HttpRequest;
import com.hpe.adm.nga.sdk.network.HttpRequestFactory;
import com.hpe.adm.nga.sdk.network.HttpResponse;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

public class SharedSpaceLevelRequestService extends AuthenticationService {

    public String getCurrentWorkspaceName() {

        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        String retVal = " ";
        try {
            UserAuthorisation auth = new UserAuthorisation(connectionSettings.getUserName(), connectionSettings.getPassword());
            HttpClient httpClient = HttpClient.getInstance();
            HttpRequestFactory requestFactory = httpClient.getRequestFactory(connectionSettings.getBaseUrl(), auth);

            HttpRequest request = requestFactory.buildGetRequest(connectionSettings.getBaseUrl() + "/api/shared_spaces/" +
                    connectionSettings.getSharedSpaceId() + "/workspaces" + "?fields = id,name" + "&query = \"users={id=" + getCurrentUserId() + "}\"");
            HttpResponse response = request.execute();
            BufferedReader buffer = new BufferedReader(new InputStreamReader(response.getContent()));
            String jsonString = buffer.lines().collect(Collectors.joining("\n"));
            JsonArray dataArray = new JsonParser().parse(jsonString).getAsJsonObject().get("data").getAsJsonArray();
            for (JsonElement elem : dataArray) {
                String id = elem.getAsJsonObject().get("id").getAsString();
                if (Long.valueOf(id).equals(connectionSettings.getWorkspaceId())) {
                    retVal = elem.getAsJsonObject().get("name").getAsString();
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return retVal;
    }
}
