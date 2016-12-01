package com.hpe.adm.octane.ideplugins.intellij.util;

public class Constants {

    public static final String IMAGE_FOLDER_PATH = "/images/";

    public static final String IMG_VENDOR_LOGO = IMAGE_FOLDER_PATH + "hpe-logo.png";
    public static final String IMG_OCTANE_LOGO = IMAGE_FOLDER_PATH + "octane-logo.png";
    public static final String IMG_OCTANE_ICON = IMAGE_FOLDER_PATH + "octane-icon-13x13.png";
    public static final String IMG_AJAX_SPINNER = IMAGE_FOLDER_PATH + "ajax-loader.gif";
    public static final String IMG_REFRESH_ICON = IMAGE_FOLDER_PATH + "refresh-16x16.png";

    //Url Parser
    public static final int INVALID_ID = -1;

    //Password encoder
    public static final String PLUGIN_SETTINGS_PASSWORD_KEY = "OCTANE_SETTINGS_PASSWORD_KEY";

    //Settings validation
    public static final String CORRECT_URL_FORMAT_MESSAGE = "Example: (http|https)://{serverurl[:port]}/?p={sharedspaceId}/{workspaceId} where sharedspaceId, workspaceId are positive numbers.";
    public static final String INVALID_URL_FORMAT_MESSAGE = "Given server URL is not valid.";

}