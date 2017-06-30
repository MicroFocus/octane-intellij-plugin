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

package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityCategory;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeModel;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;

import static com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields.FIELD_DESCRIPTION;

public class SearchResultEntityTreeCellRenderer implements TreeCellRenderer {

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

            SearchEntityModelRow rowPanel;

            if (selected && hasFocus) {
                rowPanel = new SearchEntityModelRow(new Color(255, 255, 255));
            } else {
                rowPanel = new SearchEntityModelRow();
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