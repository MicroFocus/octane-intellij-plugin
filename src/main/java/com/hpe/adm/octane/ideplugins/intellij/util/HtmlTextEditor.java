package com.hpe.adm.octane.ideplugins.intellij.util;

import com.intellij.ui.JBColor;
import com.intellij.util.ui.UIUtil;

import java.awt.*;

public class HtmlTextEditor {

    private HtmlTextEditor() {

    }

    public static String removeHtmlStructure(String htmlString) {
        htmlString = htmlString.replace("<html>", "");
        htmlString = htmlString.replace("</html>", "");
        htmlString = htmlString.replace("<body>", "");
        htmlString = htmlString.replace("</body>", "");
        htmlString = htmlString.replace("<head>", "");
        htmlString = htmlString.replace("</head>", "");
        return htmlString;
    }

    public static String getColoredHTML(final String commentContent) {
        final Color foregroundColor = JBColor.foreground();
        final Color backgroundColor = UIUtil.getLabelBackground();
        String start = "<html>\n" +
                "<body bgcolor=\"" + getHexadecimalColor(backgroundColor) + "\">\n" +
                "<font color=\"" + getHexadecimalColor(foregroundColor) + "\">";
        String end = "</font>\n" +
                "</body>\n" +
                "</html>";
        final String coloredHTMLCode = start + commentContent + end;
        return coloredHTMLCode;

    }

    private static String getHexadecimalColor(Color color) {
        int r = color.getRed();
        int g = color.getGreen();
        int b = color.getBlue();
        return String.format("#%02x%02x%02x", r, g, b);
    }

}
