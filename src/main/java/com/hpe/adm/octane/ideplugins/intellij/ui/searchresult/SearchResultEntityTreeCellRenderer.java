/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityCategory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;

import static com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields.FIELD_DESCRIPTION;

public class SearchResultEntityTreeCellRenderer implements TreeCellRenderer {

    @Inject
    private Project project;

    public SearchResultEntityTreeCellRenderer() {
        super();
    }

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {

        //Root
        if (value instanceof String) {
            return new JLabel((String) value);

        } else if (value instanceof EntityCategory) {

            EntityTreeModel model = (EntityTreeModel) tree.getModel();
            EntityCategory category = (EntityCategory) value;
            int count = model.getChildCount(value);
            String labelText = category.getName() + " (" + count + ")";

            JXLabel lbl = new JXLabel(labelText);
            Font font = new Font(lbl.getFont().getFontName(), Font.BOLD, lbl.getFont().getSize() + 4);
            lbl.setFont(font);

            if (selected && hasFocus) {
                lbl.setForeground(new Color(255, 255, 255));
            }
            return lbl;

        } else if (value instanceof EntityModel) {

            EntityModel entityModel = (EntityModel) value;
            Long entityId = Long.valueOf(Util.getUiDataFromModel(entityModel.getValue("id")));


            SearchEntityModelRow rowPanel = PluginModule.getPluginModuleForProject(project).getInstance(SearchEntityModelRow.class);
            if (selected && hasFocus) {
                rowPanel.initFocusedUI();
            } else {
                rowPanel.initUI();
            }

            rowPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, JBColor.border()));
            rowPanel.setIcon(Entity.getEntityType(entityModel), false);

            String name = Util.getUiDataFromModel(entityModel.getValue("name"));
            name = name.replace("<em>", "<b>");
            name = name.replace("</em>", "</b>");
            rowPanel.setEntityName(entityId + "", name);

            String description = Util.getUiDataFromModel(entityModel.getValue(FIELD_DESCRIPTION));
            //Remove html from description if it's not relevant to the search query
            if(description.contains("<em>")){
                description = description.replace("<em>", "<b>");
                description = description.replace("</em>", "</b>");
                rowPanel.setEntityHtmlDescription(description);
            } else {
                description = Util.stripHtml(description);
                description = Util.ellipsisTruncate(description, 100); //magic!
                rowPanel.setEntityDescription(description);
            }

            return rowPanel;
        }

        return new JLabel(value.toString());
    }

}