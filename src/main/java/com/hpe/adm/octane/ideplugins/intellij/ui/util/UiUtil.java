package com.hpe.adm.octane.ideplugins.intellij.ui.util;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeCellRenderer;
import com.hpe.adm.octane.services.filtering.Entity;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.project.Project;

/**
 * Created by marineal on 2/14/2017.
 */
public class UiUtil {

    public static void showWarningBalloon(Project project, String title, String htmlText, NotificationType type){
        Notification notification =
                new Notification(
                        "Octane IntelliJ Plugin",
                        title,
                        htmlText,
                        type,
                        null);

        notification.notify(project);
    }


    public static String entityToString(EntityModel entityModel){
        StringBuilder result = new StringBuilder();

        if(Entity.getEntityType(entityModel) != null){
            result.append(EntityTreeCellRenderer.getSubtypeName(Entity.getEntityType(entityModel).getEntityName()));
            result.append(" ");
        }

        if(entityModel.getValue("id") != null){
            result.append(entityModel.getValue("id").getValue().toString());
            result.append(": ");
        }

        if(entityModel.getValue("name") != null){
            result.append(entityModel.getValue("name").getValue().toString());
        }

        return result.toString();
    }
}
