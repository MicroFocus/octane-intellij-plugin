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

package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;

import javax.swing.*;
import java.awt.*;

public class LoadingWidget extends JPanel {

    public LoadingWidget() {
        this(null);
    }

    public LoadingWidget(String loadingMessage) {
        setLayout(new BorderLayout(0, 0));
        ImageIcon pacmanImage = new ImageIcon(LoadingWidget.class.getResource(Constants.IMG_AJAX_SPINNER));
        JLabel loadingLabel = new JLabel(pacmanImage);
        //loadingLabel.setText(loadingMessage);
        add(loadingLabel, BorderLayout.CENTER);
    }

}
