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
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.stream.Collectors;

public class OctaneExceptionHandler {

    private StringBuilder exceptionMessage;
    private NotificationBuilder notificationBuilder;

    public OctaneExceptionHandler(OctaneException ex){
        exceptionMessage = new StringBuilder();
        ex.getError().getValues().forEach( f -> {
            //display the messages as name - value pairs
            String fieldName = convertFieldNameToLabel(f.getName());
            exceptionMessage.append(fieldName + ": " + f.getValue() + "<br/>");
        });
        notificationBuilder = new NotificationBuilder(null, "ALM Octane plugin error", exceptionMessage.toString());
    }

    private String convertFieldNameToLabel(String fieldname) {
        fieldname = fieldname.replace("_", " ");
        fieldname = Arrays.stream(StringUtils.splitByCharacterTypeCamelCase(fieldname)).collect(Collectors.joining(" "));
        fieldname = Arrays.stream(fieldname.split("\\s+"))
                .map(str -> StringUtils.capitalize(str))
                .collect(Collectors.joining(" "));
        fieldname = fieldname + ": ";
        return fieldname;
    }

    public void addAction(AnAction action){
        notificationBuilder.get().addAction(action);
    }

    public void showErrorNotification(){
        notificationBuilder.type(NotificationType.ERROR);
        notificationBuilder.get().notify(notificationBuilder.getProject());
    }
}
