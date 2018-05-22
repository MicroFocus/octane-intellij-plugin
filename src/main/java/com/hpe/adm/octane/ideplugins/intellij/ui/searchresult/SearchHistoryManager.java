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

    private IdePluginPersistentState idePluginPersistentState;

    private List<String> searchHistory = new ArrayList<>();

    private final int HISTORY_SIZE = 5;

    private static SearchHistoryManager searchManager = null;

    private SearchHistoryManager(IdePluginPersistentState idePluginPersistentState){
        this.idePluginPersistentState = idePluginPersistentState;
    }

    /**
     * This method initializes the SearchHistoryManager that is going to be used application wide
     * makes sure that only one instance of manager is active
     * @param idePluginPersistentState
     */
    public static void init(IdePluginPersistentState idePluginPersistentState){
        if(searchManager == null){
            searchManager = new SearchHistoryManager(idePluginPersistentState);
        }
    }

    /**
     * This method retrieves the SearchHistoryManager
     * its return should be checked
     * @return null if the SearchHistoryManager was not initialised, otherwise returns the SearchHistoryManager
     */
    public static SearchHistoryManager getInstance(){
        return searchManager;
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

    public void loadSearchHistory() {
        JSONObject jsonObject = idePluginPersistentState.loadState(IdePluginPersistentState.Key.SEARCH_HISTORY);
        if (jsonObject == null) return;
        JSONArray jsonArray = jsonObject.getJSONArray(IdePluginPersistentState.Key.SEARCH_HISTORY.name());
        searchHistory = new ArrayList<>();
        for (int i = 0; i < jsonArray.length(); i++) {
            searchHistory.add(jsonArray.getString(i));
        }
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
        return searchHistory;
    }

    public void clearSearchHistory(){
        searchHistory = new ArrayList<>();
        saveSearchHistory();
    }
}
