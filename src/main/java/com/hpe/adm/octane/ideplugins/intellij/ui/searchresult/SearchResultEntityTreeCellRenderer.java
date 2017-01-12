package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityModelRow;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
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

        } else if (value instanceof EntityTreeModel.EntityCategory) {

            EntityTreeModel model = (EntityTreeModel) tree.getModel();
            EntityTreeModel.EntityCategory category = (EntityTreeModel.EntityCategory) value;
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
            Long entityId = Long.valueOf(UiUtil.getUiDataFromModel(entityModel.getValue("id")));

            EntityModelRow rowPanel;

            if (selected && hasFocus) {
                rowPanel = new EntityModelRow(new Color(255, 255, 255));
            } else {
                rowPanel = new EntityModelRow();
            }

            rowPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, JBColor.border()));
            rowPanel.setIcon(Entity.getEntityType(entityModel), false);

            String name = UiUtil.getUiDataFromModel(entityModel.getValue("name"));
            name = name.replace("<em>", "<b>");
            name = name.replace("</em>", "</b>");

            String description = UiUtil.getUiDataFromModel(entityModel.getValue(FIELD_DESCRIPTION));
            description = description.replace("<em>", "<b>");
            description = description.replace("</em>", "</b>");

            rowPanel.setEntityName(entityId + "", name);
            rowPanel.setEntityDetails(description, "");

            return rowPanel;
        }

        return new JLabel("N/A");
    }

}