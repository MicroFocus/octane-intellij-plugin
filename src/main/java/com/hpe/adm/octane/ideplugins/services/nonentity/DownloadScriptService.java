package com.hpe.adm.octane.ideplugins.services.nonentity;

import com.google.gson.JsonParser;
import com.hpe.adm.nga.sdk.network.HttpRequest;
import com.hpe.adm.nga.sdk.network.HttpResponse;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

/**
 * Created by dulaut on 11/25/2016.
 */
public class DownloadScriptService extends AuthenticationService {

    public String getGherkinTestScriptContent(long testId) {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

        if (authenticate()) {
            try {
                HttpRequest request = requestFactory.buildGetRequest(connectionSettings.getBaseUrl() + "/api/shared_spaces/" + connectionSettings.getSharedSpaceId() +
                        "/workspaces/" + connectionSettings.getWorkspaceId() + "/tests/" + testId + "/script");
                HttpResponse response = request.execute();

                BufferedReader buffer = new BufferedReader(new InputStreamReader(response.getContent()));
                String jsonString = buffer.lines().collect(Collectors.joining("\n"));

                String script = new JsonParser().parse(jsonString).getAsJsonObject().get("script").getAsString();
                return script;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return null;
    }
}
