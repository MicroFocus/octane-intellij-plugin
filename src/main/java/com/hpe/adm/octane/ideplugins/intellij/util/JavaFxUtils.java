package com.hpe.adm.octane.ideplugins.intellij.util;

public class JavaFxUtils {

    public static boolean isJavaFxAvailable() {
        return false;
//        try {
//            JavaFxUtils.class.getClassLoader().loadClass("javafx.embed.swing.JFXPanel");
//            return true;
//        } catch (ClassNotFoundException e) {
//            return false;
//        }
    }

}