package com.hpe.adm.octane.ideplugins.services.nonentity;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.authorisation.UserAuthorisation;
import com.hpe.adm.nga.sdk.network.HttpClient;
import com.hpe.adm.nga.sdk.network.HttpRequest;
import com.hpe.adm.nga.sdk.network.HttpRequestFactory;
import com.hpe.adm.nga.sdk.network.HttpResponse;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.exception.ServiceRuntimeException;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Set;
import java.util.stream.Collectors;

public class EntitySearchService {

    @Inject
    protected ConnectionSettingsProvider connectionSettingsProvider;

    public String searchGlobal(String queryString, Entity entity, Set<String> fields) {

        ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();

        //auth
        UserAuthorisation auth = new UserAuthorisation(connectionSettings.getUserName(), connectionSettings.getPassword());
        HttpClient httpClient = HttpClient.getInstance();
        HttpRequestFactory requestFactory = httpClient.getRequestFactory(connectionSettings.getBaseUrl(), auth);
        if(! httpClient.authenticate()){
            throw new ServiceRuntimeException("Failed to authenticate with current connection settings");
        }

        StringBuilder urlBuilder = new StringBuilder();

        urlBuilder.append(connectionSettings.getBaseUrl() + "/api");
        urlBuilder.append("/shared_spaces/" + connectionSettings.getSharedSpaceId());
        urlBuilder.append("/workspaces/" + connectionSettings.getWorkspaceId());
        urlBuilder.append("/" + entity.getApiEntityName());

        urlBuilder.append("?text_search={\"type\":\"global\",\"text\":\""+queryString+"\"}");

        if(fields != null && fields.size() != 0) {
            urlBuilder.append("&fields=" + String.join(",", fields));
        }
//        if(entity.isSubtype()) {
//            urlBuilder.append("&query=" + "\"(subtype='" + entity.getSubtypeName() + "')\"");
//        }

        try {
            String url = encodeUrl(urlBuilder.toString());
            System.out.println(url);
            HttpRequest request = requestFactory.buildGetRequest(url);
            HttpResponse response = request.execute();
            BufferedReader buffer = new BufferedReader(new InputStreamReader(response.getContent()));
            String responseString = buffer.lines().collect(Collectors.joining("\n"));

            return responseString;

        } catch (IOException ex) {
            throw new ServiceRuntimeException(ex);
        }
    }

    private static String encodeUrl(String urlString){
        try {
            URL url = new URL(urlString);
            URI uri = new URI(url.getProtocol(), url.getUserInfo(), url.getHost(), url.getPort(), url.getPath(), url.getQuery(), url.getRef());
            return uri.toASCIIString();
        } catch (MalformedURLException | URISyntaxException e) {
            return null;
        }
    }

}
