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

package com.hpe.adm.octane.ideplugins.intellij.util;

import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.project.Project;

public class ExceptionHandler {

    private StringBuilder exceptionMessage;
    private NotificationBuilder notificationBuilder;

    public ExceptionHandler(OctaneException ex, Project project){
        exceptionMessage = new StringBuilder();
        ex.getError().getValues().forEach( f -> {
            //display the messages as name - value pairs
            String fieldName = UiUtil.convertFieldNameToLabel(f.getName());
            exceptionMessage.append(fieldName + ": " + f.getValue() + "<br/>");
        });
        notificationBuilder = new NotificationBuilder(project, "Core Software Delivery Platform plugin error", exceptionMessage.toString());
    }

    public ExceptionHandler(Exception ex, Project project){
        exceptionMessage = new StringBuilder();
        exceptionMessage.append(ex.getMessage());
        notificationBuilder = new NotificationBuilder(project, "Core Software Delivery Platform plugin error", exceptionMessage.toString());
    }

    public void addAction(AnAction action){
        notificationBuilder.get().addAction(action);
    }

    public void showErrorNotification(){
        notificationBuilder.type(NotificationType.ERROR);
        notificationBuilder.get().notify(notificationBuilder.getProject());
    }
}
