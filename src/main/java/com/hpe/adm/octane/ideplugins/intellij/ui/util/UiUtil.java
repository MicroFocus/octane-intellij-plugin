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

package com.hpe.adm.octane.ideplugins.intellij.ui.util;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeCellRenderer;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationAction;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.project.Project;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Created by marineal on 2/14/2017.
 */
public class UiUtil {

    public static void showWarningBalloon(Project project, String title, String htmlText, NotificationType type) {
        showWarningBalloon(project, title, htmlText, type, null);
    }

    public static void showWarningBalloon(Project project, String title, String htmlText, NotificationType type, NotificationAction notificationAction) {
        Notification notification =
                new Notification(
                        "ValueEdge IntelliJ Plugin",
                        title,
                        htmlText,
                        type);

        if (notificationAction != null) {
            notification.addAction(notificationAction);
        }

        notification.notify(project);
    }


    public static String entityToString(EntityModel entityModel) {
        StringBuilder result = new StringBuilder();

        if (Entity.getEntityType(entityModel) != null) {
            result.append(EntityTreeCellRenderer.getEntityDisplayName(Entity.getEntityType(entityModel).getEntityName()));
            result.append(" ");
        }

        if (entityModel.getValue("id") != null) {
            result.append(entityModel.getValue("id").getValue().toString());
            result.append(": ");
        }

        if (entityModel.getValue("name") != null) {
            result.append(entityModel.getValue("name").getValue().toString());
        }

        return result.toString();
    }

    public static String convertFieldNameToLabel(String fieldname) {
        fieldname = fieldname.replace("_", " ");
        fieldname = Arrays.stream(StringUtils.splitByCharacterTypeCamelCase(fieldname)).collect(Collectors.joining(" "));
        fieldname = Arrays.stream(fieldname.split("\\s+"))
                .map(str -> StringUtils.capitalize(str))
                .collect(Collectors.joining(" "));
        fieldname = fieldname + ": ";
        return fieldname;
    }
}
