/*
 * © Copyright 2017-2022 Micro Focus or one of its affiliates.
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

package com.hpe.adm.octane.ideplugins.intellij.ui.entityicon;

import java.awt.*;

class IconDetail{

    private Color color;
    private String displayLabelText;
    private boolean isOpaque = true;

    public IconDetail(Color color, String displayLabelText) {
        this.color = color;
        this.displayLabelText = displayLabelText;
    }

    public IconDetail(Color color, String displayLabelText, boolean isOpaque) {
        this.color = color;
        this.displayLabelText = displayLabelText;
        this.isOpaque = isOpaque;
    }

    public Color getColor() {
        return color;
    }

    public String getDisplayLabelText() {
        return displayLabelText;
    }

    public boolean isOpaque() {
        return isOpaque;
    }
}
