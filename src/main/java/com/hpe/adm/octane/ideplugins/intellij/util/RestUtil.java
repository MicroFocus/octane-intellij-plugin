/*
 * © 2018 EntIT Software LLC, a Micro Focus company, L.P.
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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.util.Consumer;
import org.jetbrains.annotations.NotNull;

import java.util.function.Supplier;

public class RestUtil {

    public static String LOADING_MESSAGE = "Fetching data from Octane...";

    private static NotificationUtil notificationService = new NotificationUtil();

    public static <T> void runInBackground(final Supplier<T> supplier, final Consumer<T> consumer, final Project project) {
        runInBackground(supplier, consumer, project, null);
    }

    /**
     * @param errorMessage if the provided supplier throws an exception, this error message is displayed (if it is not null)
     *                     and the provided consumer will not be executed.
     */
    public static <T> void runInBackground(final Supplier<T> supplier,
                                           final Consumer<T> consumer,
                                           final Project project,
                                           final String errorMessage) {

        runInBackground(supplier, consumer, project, errorMessage, LOADING_MESSAGE);
    }

    /**
     * @param errorMessage if the provided supplier throws an exception, this error message is displayed (if it is not null)
     *                     and the provided consumer will not be executed.
     */
    public static <T> void runInBackground(final Supplier<T> supplier,
                                           final Consumer<T> consumer,
                                           final Project project,
                                           final String errorMessage,
                                           final String loadingMessage) {
        ApplicationManager.getApplication().invokeLater(() -> {
            Task.Backgroundable backgroundTask = new Task.Backgroundable(project, loadingMessage, true) {
                public void run(@NotNull ProgressIndicator indicator) {
                    try {
                        final T result = supplier.get();
                        ApplicationManager.getApplication().invokeLater(() -> {
                            //noinspection unchecked
                            consumer.consume(result);
                        });
                    } catch (RuntimeException e) {
                        if (errorMessage != null) {
                            notifyError(e, errorMessage, project);
                        } else {
                            throw e;
                        }
                    }
                }
            };
            backgroundTask.queue();
        });
    }

    private static void notifyError(Throwable throwable, String errorMessage, Project project) {
        NotificationBuilder notification = new NotificationBuilder(project, errorMessage, getErrorTextFromException(throwable));
        notificationService.notifyError(notification);
    }

    private static String getErrorTextFromException(Throwable t) {
        String message = t.getMessage();
        if (message == null) {
            message = "(No exception message available)";
            //log.error(message, t);
        }
        return message;
    }


}
