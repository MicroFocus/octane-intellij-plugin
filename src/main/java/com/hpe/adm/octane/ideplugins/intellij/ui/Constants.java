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

package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.intellij.util.ui.UIUtil;

public class Constants {

    public static final String IMG_FOLDER_PATH = "/images/";
    public static final String IMG_VENDOR_LOGO_LIGHT = IMG_FOLDER_PATH + "opentext-logo_light-theme-128.png";
    public static final String IMG_VENDOR_LOGO_DARK = IMG_FOLDER_PATH + "opentext-logo_dark-theme-128.png";
    public static final String IMG_OCTANE_LOGO = IMG_FOLDER_PATH + "octane-logo.png";
    public static final String IMG_OCTANE_LOGO_20x20 = IMG_FOLDER_PATH + "octane-logo-20x20.png";
    public static final String IMG_SPINNER_LIGHT = IMG_FOLDER_PATH + "octane_preloader_light-128.gif";
    public static final String IMG_SPINNER_DARK = IMG_FOLDER_PATH + "octane_preloader_dark-128.gif";
    public static final String IMG_REFRESH_ICON = IMG_FOLDER_PATH + "refresh-16x16.png";
    public static final String IMG_COMMENTS_ICON = IMG_FOLDER_PATH + "comments-16x16.png";
    public static final String IMG_BROWSER_ICON = IMG_FOLDER_PATH + "browser-16x16.png";
    public static final String IMG_START_TIMER = IMG_FOLDER_PATH + "startTimer-16x16.png";
    public static final String IMG_STOP_TIMER = IMG_FOLDER_PATH + "stopTimer-16x16.png";
    public static final String IMG_PLAIN_ROBOT = IMG_FOLDER_PATH + "s-no-items-to-display.png";
    public static final String IMG_ACTIVE_ITEM = IMG_FOLDER_PATH + "activeitem.png";
    public static final String IMG_ACTIVE_ITEM_20x20 = IMG_FOLDER_PATH + "activeitem-20x20.png";
    public static final String IMG_MYWORK = IMG_FOLDER_PATH + "mywork.png";
    public static final String IMG_FIELD_SELECTION_NON_DEFAULT = IMG_FOLDER_PATH + "cus-on.png";
    public static final String IMG_FIELD_SELECTION_DEFAULT = IMG_FOLDER_PATH + "cus-off.png";
    public static final String IMG_NO_WORK_ROCKET = IMG_FOLDER_PATH + "s-rocket.png";
    public static final String IMG_PHASE_DROPDOWN = IMG_FOLDER_PATH + "drop-down.png";
    public static final String IMG_PHASE_DROPDOWN_DARKULA = IMG_FOLDER_PATH + "drop-down-darkula.png";
    public static final String IMG_ENTITY_COMBOBOX_ARROW = IMG_FOLDER_PATH +"arrow.png";
    public static final String IMG_COPY_ICON = IMG_FOLDER_PATH + "copy-icon.png";
    public static final String IMG_HELP_ICON = IMG_FOLDER_PATH + "help-16x16.png";
    public static final String IMG_SEARCH_ICON = IMG_FOLDER_PATH + "search.png";

    public static final String IMG_TRANSPARENT = IMG_FOLDER_PATH + "transparent_1x1.png";

    //Settings validation
    public static final String CORRECT_URL_FORMAT_MESSAGE = "Example: (http|https)://{serverurl[:port]}/?p={sharedspaceId}/{workspaceId}";

    //Tabs title
    public static final String TAB_MY_WORK_TITLE = "My work";

    public static String getOctaneRemoveIcon() {
        if (UIUtil.isUnderDarcula()) {
            return IMG_FOLDER_PATH + "octane_remove_light.png";
        } else {
            return IMG_FOLDER_PATH + "octane_remove_dark.png";
        }
    }

    public static String getOctaneRemoveDisabledIcon() {
        if (UIUtil.isUnderDarcula()) {
            return IMG_TRANSPARENT;
        } else {
            return IMG_FOLDER_PATH + "octane_remove_light.png";
        }
    }



}