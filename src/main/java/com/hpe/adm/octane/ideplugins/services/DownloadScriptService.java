package com.hpe.adm.octane.ideplugins.services;

import com.google.gson.JsonParser;
import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.network.HttpClient;
import com.hpe.adm.nga.sdk.network.HttpRequest;
import com.hpe.adm.nga.sdk.network.HttpRequestFactory;
import com.hpe.adm.nga.sdk.network.HttpResponse;
import com.hpe.adm.octane.ideplugins.services.ServiceBase;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

/**
 * Created by dulaut on 11/25/2016.
 */
public class DownloadScriptService extends ServiceBase {

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    public String getGherkinTestScriptContent(int testId) {
        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
        System.out.println("url = " + connectionSettings.getBaseUrl());
        UserAuthorisation auth = new UserAuthorisation(connectionSettings.getUserName(), connectionSettings.getPassword());

        HttpClient httpClient = HttpClient.getInstance();
        HttpRequestFactory requestFactory = httpClient.getRequestFactory(connectionSettings.getBaseUrl(), auth);
        if (httpClient.authenticate()) {
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
