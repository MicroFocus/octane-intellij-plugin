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
        notificationBuilder = new NotificationBuilder(project, "ALM Octane plugin error", exceptionMessage.toString());
    }

    public ExceptionHandler(Exception ex, Project project){
        exceptionMessage = new StringBuilder();
        exceptionMessage.append(ex.getMessage());
        notificationBuilder = new NotificationBuilder(project, "ALM Octane plugin error", exceptionMessage.toString());
    }

    public void addAction(AnAction action){
        notificationBuilder.get().addAction(action);
    }

    public void showErrorNotification(){
        notificationBuilder.type(NotificationType.ERROR);
        notificationBuilder.get().notify(notificationBuilder.getProject());
    }
}
