package com.hpe.adm.octane.ideplugins.intellij.util;

public class JavaFxUtils {

    public static boolean isJavaFxAvailable() {
        try {
            JavaFxUtils.class.getClassLoader().loadClass("javafx.embed.swing.JFXPanel");
            return true;
        } catch (ClassNotFoundException e) {
            return false;
        }
    }

    /**
     * JDK bug, might be missing in some versions of jbr
     */
    public static boolean isJavaFxInteropFactoryNAvailable() {
        try {
            JavaFxUtils.class.getClassLoader().loadClass("com.sun.javafx.embed.swing.newimpl.InteropFactoryN");
            return true;
        } catch (ClassNotFoundException e) {
            return false;
        }
    }

}