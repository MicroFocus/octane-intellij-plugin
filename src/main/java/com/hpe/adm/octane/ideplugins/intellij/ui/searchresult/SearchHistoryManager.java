/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

@Singleton
public class SearchHistoryManager {

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private List<String> searchHistory;

    private final int HISTORY_SIZE = 5;

    public SearchHistoryManager() {
    }

    private void saveSearchHistory() {
        JSONArray jsonArray = new JSONArray();
        searchHistory.forEach(searchQuery -> jsonArray.put(searchQuery));
        JSONObject jsonObject = new JSONObject();
        jsonObject.put(IdePluginPersistentState.Key.SEARCH_HISTORY.name(), jsonArray);

        idePluginPersistentState.saveState(
                IdePluginPersistentState.Key.SEARCH_HISTORY,
                jsonObject);
    }

    public void addToSearchHistory(String string) {
        if (searchHistory == null) {
            loadSearchHistory();
        }
        if (searchHistory.contains(string)) {
            searchHistory.remove(string);
        }
        searchHistory.add(0, string);
        if (searchHistory.size() > HISTORY_SIZE) {
            searchHistory.remove(HISTORY_SIZE);
        }
        saveSearchHistory();
    }

    public List<String> getSearchHistory() {
        if (searchHistory == null) {
            loadSearchHistory();
        }
        return searchHistory;
    }

    private void loadSearchHistory() {
        JSONObject jsonObject = idePluginPersistentState.loadState(IdePluginPersistentState.Key.SEARCH_HISTORY);
        searchHistory = new ArrayList<>();

        // If the history is not present, it's because the plugin is being upgraded from a version that did not have the persistent history feature,
        // in this case we just leave the searchHistory list empty to avoid a null pointer exception
        if (jsonObject != null) {
            JSONArray jsonArray = jsonObject.getJSONArray(IdePluginPersistentState.Key.SEARCH_HISTORY.name());
            for (int i = 0; i < jsonArray.length(); i++) {
                searchHistory.add(jsonArray.getString(i));
            }
        }
    }

    public void clearSearchHistory() {
        searchHistory = new ArrayList<>();
        saveSearchHistory();
    }
}
