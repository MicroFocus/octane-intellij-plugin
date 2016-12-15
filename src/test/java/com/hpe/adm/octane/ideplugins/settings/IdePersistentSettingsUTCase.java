package com.hpe.adm.octane.ideplugins.settings;

import com.hpe.adm.octane.ideplugins.intellij.settings.IdePersistentSettings;
import org.jdom.output.XMLOutputter;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

public class IdePersistentSettingsUTCase {

    @Test
    public void testSettingsStateManagement(){
        //Add some stuff to a settings object
        IdePersistentSettings settings1 = new IdePersistentSettings();
        JSONObject json = new JSONObject("{\"value\": \"test\"}");
        settings1.setSetting(IdePersistentSettings.Key.OPEN_TABS, json);
        settings1.setSetting(IdePersistentSettings.Key.ACTIVE_WORK_ITEM, json);

        //Create a new settings object, load it's state from the prev
        IdePersistentSettings settings2 = new IdePersistentSettings();
        settings2.loadState(settings1.getState());

        //assert key are equal
        Assert.assertEquals(
                settings1.getSetting(IdePersistentSettings.Key.OPEN_TABS).toString(),
                settings2.getSetting(IdePersistentSettings.Key.OPEN_TABS).toString());
        Assert.assertEquals(
                settings1.getSetting(IdePersistentSettings.Key.ACTIVE_WORK_ITEM).toString(),
                settings2.getSetting(IdePersistentSettings.Key.ACTIVE_WORK_ITEM).toString());

        //check if the states are equal
        XMLOutputter outputter = new XMLOutputter();
        String settings1String = outputter.outputString(settings1.getState());
        String settings2String = outputter.outputString(settings2.getState());
        Assert.assertEquals(settings1String, settings2String);
    }

}
