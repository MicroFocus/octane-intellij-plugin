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

package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jetbrains.annotations.Nullable;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@State(
    name = "OctanePluginState"
)
public class IdePluginPersistentState implements PersistentStateComponent<Element> {

    private static final String OCTANE_PLUGIN_STATE_TAG = "OctanePluginState";

    private List<SettingsChangedHandler> changedHandlers = new ArrayList<>();
    private Map<Key, JSONObject> stateMap = new HashMap<>();

    public interface SettingsChangedHandler {
        void stateChanged(Key key, JSONObject value);
    }

    public enum Key {
        ACTIVE_WORK_ITEM,
        PREV_ACTIVE_WORK_ITEM,
        OPEN_TABS,
        SELECTED_TAB,
        SEARCH_HISTORY,
        EXPECTED_COMMIT_MESSAGE,
        SELECTED_FIELDS
    }

    public void saveState(Key key, JSONObject value){
        stateMap.put(key, value);
        changedHandlers.forEach(changedHandler -> changedHandler.stateChanged(key, value));
    }

    public void clearState(Key key){
        if(stateMap.containsKey(key)) {
            stateMap.remove(key);
            changedHandlers.forEach(changedHandler -> changedHandler.stateChanged(key, null));
        }
    }

    public JSONObject loadState(Key key){
        return stateMap.get(key);
    }

    public void addStateChangedHandler(SettingsChangedHandler changedHandler){
        changedHandlers.add(changedHandler);
    }

    public boolean removeStateChangedHandler(SettingsChangedHandler changedHandler){
        return changedHandlers.remove(changedHandler);
    }

    @Nullable
    @Override
    public Element getState() {
        final Element element = new Element(OCTANE_PLUGIN_STATE_TAG);
        for(Key key : stateMap.keySet()){
            element.setAttribute(key.name(), stateMap.get(key).toString());
        }
        return element;
    }

    @Override
    public void loadState(Element state) {
        stateMap.clear();
        for(Attribute attribute : state.getAttributes()){
            Key key = Key.valueOf(attribute.getName());
            String value = state.getAttributeValue(attribute.getName());
            stateMap.put(key, new JSONObject(value));
        }
    }

}