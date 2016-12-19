package com.hpe.adm.octane.ideplugins.settings;

import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import org.jdom.output.XMLOutputter;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

public class IdePersistentSettingsUTCase {

    @Test
    public void testSettingsStateManagement(){
        //Add some stuff to a settings object
        IdePluginPersistentState settings1 = new IdePluginPersistentState();
        JSONObject json = new JSONObject("{\"value\": \"test\"}");
        settings1.saveState(IdePluginPersistentState.Key.OPEN_TABS, json);
        settings1.saveState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM, json);

        //Create a new settings object, load it's state from the prev
        IdePluginPersistentState settings2 = new IdePluginPersistentState();
        settings2.loadState(settings1.getState());

        //assert key are equal
        Assert.assertEquals(
                settings1.loadState(IdePluginPersistentState.Key.OPEN_TABS).toString(),
                settings2.loadState(IdePluginPersistentState.Key.OPEN_TABS).toString());
        Assert.assertEquals(
                settings1.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM).toString(),
                settings2.loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM).toString());

        //check if the states are equal
        XMLOutputter outputter = new XMLOutputter();
        String settings1String = outputter.outputString(settings1.getState());
        String settings2String = outputter.outputString(settings2.getState());
        Assert.assertEquals(settings1String, settings2String);
    }

}
