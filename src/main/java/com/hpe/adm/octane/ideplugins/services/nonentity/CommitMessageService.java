package com.hpe.adm.octane.ideplugins.services.nonentity;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.hpe.adm.nga.sdk.network.HttpRequest;
import com.hpe.adm.nga.sdk.network.HttpResponse;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettings;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.apache.commons.net.bsd.RCommandClient;
import org.junit.Assert;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by dulaut on 12/14/2016.
 */
public class CommitMessageService extends AuthenticationService {

    public boolean validateCommitMessage(String commitMessage, Entity entityType, long entityId) {
        String type;
        switch (entityType) {
            case DEFECT:
                type = "defect";
                break;
            case USER_STORY:
                type = "story";
                break;
            default:
                return false;
        }
        if (authenticate()) {
            try {
                ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
                HttpRequest request = requestFactory.buildGetRequest(connectionSettings.getBaseUrl() + "/internal-api/shared_spaces/" +
                        connectionSettings.getSharedSpaceId() + "/workspaces/" + connectionSettings.getWorkspaceId() +
                        "/ali/validateCommitPattern?comment=" + URLEncoder.encode(commitMessage, "UTF-8"));

                System.out.println(request.getUrl().toString());

                HttpResponse response = request.execute();

                BufferedReader buffer = new BufferedReader(new InputStreamReader(response.getContent()));
                String jsonString = buffer.lines().collect(Collectors.joining("\n"));

                JsonArray matchedIdsArray = new JsonParser().parse(jsonString).getAsJsonObject().get(type).getAsJsonArray();
                for (JsonElement element : matchedIdsArray) {
                    if (element.getAsLong() == entityId) {
                        return true;
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return false;
    }

    public List<String> getCommitPatternsForStoryType(Entity entityType) {
        String type;
        switch (entityType) {
            case DEFECT:
                type = "Defect";
                break;
            case USER_STORY:
                type = "User story";
                break;
            default:
                return null;
        }

        List<String> commitPatterns = new ArrayList<>();
        if (authenticate()) {
            try {
                ConnectionSettings connectionSettings = connectionSettingsProvider.getConnectionSettings();
                HttpRequest request = requestFactory.buildGetRequest(connectionSettings.getBaseUrl() + "/api/shared_spaces/" + connectionSettings.getSharedSpaceId() +
                        "/workspaces/" + connectionSettings.getWorkspaceId() + "/scm_commit_patterns");
                HttpResponse response = request.execute();

                BufferedReader buffer = new BufferedReader(new InputStreamReader(response.getContent()));
                String jsonString = buffer.lines().collect(Collectors.joining("\n"));

                JsonArray dataArray = new JsonParser().parse(jsonString).getAsJsonObject().get("data").getAsJsonArray();
                for (JsonElement elem : dataArray) {
                    String name = elem.getAsJsonObject().get("entity_type").getAsJsonObject().get("name").getAsString();
                    if (name.equals(type)) {
                        commitPatterns.add(elem.getAsJsonObject().get("pattern").getAsString());
                    }
                }
                return commitPatterns;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    @Test
    public void test() {
//        List<String> patterns = getCommitPatternsForStoryType(Entity.USER_STORY);
//        System.out.println(patterns);
        boolean match = validateCommitMessage("story 15 bla bla 23(d)", Entity.DEFECT, 23);
        Assert.assertTrue(match);
    }
}