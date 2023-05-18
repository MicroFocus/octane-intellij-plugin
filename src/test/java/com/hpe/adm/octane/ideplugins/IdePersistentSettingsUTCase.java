/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
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

package com.hpe.adm.octane.ideplugins;

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
