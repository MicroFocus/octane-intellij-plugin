/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class SearchHistoryManager {

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private List<String> searchHistory = new ArrayList<>();

    private final int HISTORY_SIZE = 5;

    public SearchHistoryManager(){
    }

    public void saveSearchHistory() {
        JSONArray jsonArray = new JSONArray();
        searchHistory.forEach(searchQuery -> jsonArray.put(searchQuery));
        JSONObject jsonObject = new JSONObject();
        jsonObject.put(IdePluginPersistentState.Key.SEARCH_HISTORY.name(), jsonArray);

        idePluginPersistentState.saveState(
                IdePluginPersistentState.Key.SEARCH_HISTORY,
                jsonObject);
    }

    public void addToSearchHistory(String string) {
        if (searchHistory.contains(string)) {
            searchHistory.remove(string);
        }
        searchHistory.add(0, string);
        if (searchHistory.size() > HISTORY_SIZE) {
            searchHistory.remove(HISTORY_SIZE);
        }
    }

    public List<String> getSearchHistory(){
        JSONObject jsonObject = idePluginPersistentState.loadState(IdePluginPersistentState.Key.SEARCH_HISTORY);
        if (jsonObject == null)
            return null;
        JSONArray jsonArray = jsonObject.getJSONArray(IdePluginPersistentState.Key.SEARCH_HISTORY.name());
        searchHistory = new ArrayList<>();
        for (int i = 0; i < jsonArray.length(); i++) {
            searchHistory.add(jsonArray.getString(i));
        }
        return searchHistory;
    }

    public void clearSearchHistory(){
        searchHistory = new ArrayList<>();
        saveSearchHistory();
    }
}
