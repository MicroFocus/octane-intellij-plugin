package com.hpe.adm.octane.ideplugins.intellij.util;

import com.intellij.ide.passwordSafe.PasswordSafe;
import com.intellij.ide.passwordSafe.PasswordSafeException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class PasswordEncoder {

    public static void encryptPassword(final String password, @Nullable Project project, @NotNull Class requestor, String key) {
        try {
            PasswordSafe.getInstance().storePassword(project, requestor, key, password != null ? password : "");
        } catch (PasswordSafeException e) {
//            log.info("Couldn't set password for key [" + GERRIT_SETTINGS_PASSWORD_KEY + "]", e);
        }
    }

    public static String decryptPassword(@Nullable Project project, @NotNull Class requestor, String key) {
        String password;
        try {
            password = PasswordSafe.getInstance().getPassword(project, requestor, key);
        } catch (PasswordSafeException e) {
//            log.info("Couldn't get password for key [" + key + "]", e);
            password = "";
        }
        return StringUtil.notNullize(password);
    }
}
