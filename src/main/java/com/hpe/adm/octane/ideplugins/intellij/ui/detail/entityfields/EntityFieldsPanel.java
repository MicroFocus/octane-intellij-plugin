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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.FieldEditor;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.FieldEditorFactory;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Collection;
import java.util.Set;

@SuppressWarnings("serial")
public class EntityFieldsPanel extends JXPanel {

    private Collection<FieldMetadata> fields;
    private JXPanel detailsRightPanel;
    private JXPanel detailsLeftPanel;
    private JXLabel generalLabel;

    @Inject
    private FieldEditorFactory fieldFactory;

    public EntityFieldsPanel() {
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0,0};
        gbl.columnWeights = new double[]{0.0, 0.0};
        gbl.rowHeights = new int[]{0,0};
        gbl.rowWeights = new double[]{0.0,0.0};
        setLayout(gbl);

        generalLabel = new JXLabel("General");
        generalLabel.setFont(new Font(generalLabel.getFont().getName(), Font.BOLD, 18));
        GridBagConstraints gbc_GeneralTitle = new GridBagConstraints();
        gbc_GeneralTitle.anchor = GridBagConstraints.WEST;
        gbc_GeneralTitle.insets = new Insets(5,0, 10, 0);
        gbc_GeneralTitle.gridx = 0;
        gbc_GeneralTitle.gridy = 0;
        add(generalLabel, gbc_GeneralTitle);

        detailsLeftPanel = new JXPanel();
        GridBagLayout gbl_detailsLeftPanel = new GridBagLayout();
        gbl_detailsLeftPanel.columnWeights = new double[]{0.0, 0.0};
        detailsLeftPanel.setLayout(gbl_detailsLeftPanel);
        GridBagConstraints gbc_leftPanel = new GridBagConstraints();
        gbc_leftPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_leftPanel.anchor = GridBagConstraints.WEST;
        gbc_leftPanel.gridx = 0;
        gbc_leftPanel.gridy = 1;
        gbc_leftPanel.weightx = 1.0;
        add(detailsLeftPanel, gbc_leftPanel);


        detailsRightPanel = new JXPanel();
        GridBagLayout gbl_detailsRightPanel = new GridBagLayout();
        gbl_detailsRightPanel.columnWeights = new double[]{0.0, 0.0};
        detailsRightPanel.setLayout(gbl_detailsRightPanel);
        GridBagConstraints gbc_rightPanel = new GridBagConstraints();
        gbc_rightPanel.fill = GridBagConstraints.HORIZONTAL;
        gbc_rightPanel.anchor = GridBagConstraints.WEST;
        gbc_rightPanel.gridx = 1;
        gbc_rightPanel.gridy = 1;
        gbc_rightPanel.weightx = 1.0;
        add(detailsRightPanel, gbc_rightPanel);

        addComponentListener(detailsLeftPanel, detailsRightPanel);
    }

    public void setFields(Collection<FieldMetadata> fields) {
        this.fields = fields;
    }

    public void addComponentListener(JXPanel detailsLeftPanel, JXPanel detailsRightPanel) {
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                int halfWidth = getWidth() / 2;
                int height = getHeight();
                if (halfWidth != 0 && height != 0) {
                    Dimension halfSizeFields = new Dimension((int) halfWidth, height);
                    detailsLeftPanel.setPreferredSize(halfSizeFields);
                    detailsRightPanel.setPreferredSize(halfSizeFields);
                    updateUI();
                    repaint();
                }
            }
        });
    }

    public void setEntityModel(EntityModelWrapper entityModelWrapper, Set<String> fieldNames) {
        detailsLeftPanel.removeAll();
        detailsRightPanel.removeAll();

        int fieldCount = 0;
        int i = 0;
        for (FieldMetadata fieldMetadata : fields) {
            if (fieldNames.contains(fieldMetadata.getName())) {
                String fieldName = fieldMetadata.getName();
                JXLabel fieldLabel = new JXLabel();
                Font font = new Font(fieldLabel.getFont().getFontName(), Font.BOLD, fieldLabel.getFont().getSize());
                fieldLabel.setFont(font);
                fieldLabel.setText(fieldMetadata.getLabel());
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.anchor = GridBagConstraints.WEST;
                gbc1.fill = GridBagConstraints.HORIZONTAL;
                gbc1.insets = new Insets(0,0,5, 0);
                gbc1.gridx = 0;
                gbc1.gridy = i;

                FieldEditor fieldValueLabel = fieldFactory.createFieldEditor(entityModelWrapper, fieldName);
                GridBagConstraints gbc2 = new GridBagConstraints();
                gbc2.insets = new Insets(0, 10, 5, 5);
                gbc2.anchor = GridBagConstraints.WEST;
                gbc2.fill = GridBagConstraints.HORIZONTAL;
                gbc2.gridx = 1;
                gbc2.gridy = i;
                gbc2.weightx = 1.0;

                if (fieldCount % 2 == 0) {
                    detailsLeftPanel.add(fieldLabel, gbc1);
                    detailsLeftPanel.add(fieldValueLabel, gbc2);
                } else {
                    detailsRightPanel.add(fieldLabel, gbc1);
                    detailsRightPanel.add(fieldValueLabel, gbc2);
                }
                i++;
                fieldCount++;
            }
        }

        if (fieldCount % 2 == 1) {
            JXLabel emptyLabel = new JXLabel();
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.insets = new Insets(0, 10, 5, 5);
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.fill = GridBagConstraints.BOTH;
            gbc.gridx = 0;
            gbc.gridy = i;
            gbc.gridwidth = 2;
            detailsRightPanel.add(emptyLabel, gbc);
        }
        detailsLeftPanel.repaint();
        detailsLeftPanel.revalidate();
        detailsRightPanel.repaint();
        detailsRightPanel.revalidate();
    }
}
