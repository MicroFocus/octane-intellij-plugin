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
        OPEN_TABS,
        SELECTED_TAB,
        SEARCH_HISTORY
    }

    public void saveState(Key key, JSONObject value){
        stateMap.put(key, value);
        changedHandlers.forEach(changedHandler -> changedHandler.stateChanged(key, value));
    }

    public void clearState(Key key){
        stateMap.remove(key);
        changedHandlers.forEach(changedHandler -> changedHandler.stateChanged(key, null));
    }

    public JSONObject loadState(Key key){
        return stateMap.get(key);
    }

    public void addStateChangedHandler(SettingsChangedHandler changedHandler){
        changedHandlers.add(changedHandler);
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