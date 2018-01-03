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

package com.hpe.adm.octane.ideplugins.intellij.ui.main;

import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;

import javax.swing.*;

public class MainView implements View {

    private final JPanel rootPanel = new JPanel();

    public MainView(){
        GridLayoutManager gridLayoutManager = new GridLayoutManager(1, 1);
        rootPanel.setLayout(gridLayoutManager);
    }

    public void setTabView(View view){
        GridConstraints gc = new GridConstraints();
        gc.setFill(GridConstraints.FILL_BOTH);
        rootPanel.add(view.getComponent(), gc);
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

}
