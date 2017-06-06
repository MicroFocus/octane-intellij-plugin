/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
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
