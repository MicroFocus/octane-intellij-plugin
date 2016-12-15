package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jetbrains.annotations.Nullable;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@State(
    name = "OctanePlugin",
    storages = {
        @Storage(
                file = "$APP_CONFIG$/octane_settings.xml"
        )}
)
public class IdePersistentSettings implements PersistentStateComponent<Element>, Serializable {

    private static final String SETTINGS_TAG = "PluginSettings";

    private List<SettingsChangedHandler> changedHandlers = new ArrayList<>();

    private Map<Key, JSONObject> settingsMap = new HashMap<>();

    public interface SettingsChangedHandler {
        void settingsChanged(Key key, JSONObject value);
    }

    public enum Key {
        ACTIVE_WORK_ITEM,
        OPEN_TABS,
    }

    public void setSetting(Key key, JSONObject value){
        settingsMap.put(key, value);
        changedHandlers.forEach(changedHandler -> changedHandler.settingsChanged(key, value));
    }

    public JSONObject getSetting(Key key){
        return settingsMap.get(key);
    }

    public void addSettingsChangedHandler(SettingsChangedHandler changedHandler){
        changedHandlers.add(changedHandler);
    }

    @Nullable
    @Override
    public Element getState() {
        final Element element = new Element(SETTINGS_TAG);
        for(Key key : settingsMap.keySet()){
            element.setAttribute(key.name(), settingsMap.get(key).toString());
        }
        return element;
    }

    @Override
    public void loadState(Element state) {
        settingsMap.clear();
        for(Attribute attribute : state.getAttributes()){
            Key key = Key.valueOf(attribute.getName());
            String value = state.getAttributeValue(attribute.getName());
            settingsMap.put(key, new JSONObject(value));
        }
    }

}