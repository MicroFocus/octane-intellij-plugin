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

import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.JBColor;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.border.MatteBorder;
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

    public EntityFieldsPanel() {
        setLayout(new MigLayout("", "[pref!][10px][pref!]", "[23px,top][263px]"));

        generalLabel = new JXLabel("General");
        generalLabel.setFont(new Font("Arial", Font.BOLD, 18));
        add(generalLabel, "cell 0 0 3 1,growx,aligny center");

        detailsLeftPanel = new JXPanel();
        add(detailsLeftPanel, "cell 0 1,width 50%!,growy");
        GridBagLayout gbl_detailsLeftPanel = new GridBagLayout();
        detailsLeftPanel.setLayout(gbl_detailsLeftPanel);

        detailsRightPanel = new JXPanel();
        add(detailsRightPanel, "cell 2 1,width 50%!,growy");
        GridBagLayout gbl_detailsRightPanel = new GridBagLayout();
        detailsRightPanel.setLayout(gbl_detailsRightPanel);

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
                fieldLabel.setFont(new Font("Arial", Font.BOLD, 12));
                fieldLabel.setText(fieldMetadata.getLabel());
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.anchor = GridBagConstraints.FIRST_LINE_START;
                gbc1.insets = new Insets(10, 0, 0, 0);
                gbc1.fill = GridBagConstraints.HORIZONTAL;
                gbc1.gridx = 0;
                gbc1.gridy = i;

                String fieldValue = null;

                if (DetailsViewDefaultFields.FIELD_OWNER.equals(fieldName)
                        || DetailsViewDefaultFields.FIELD_AUTHOR.equals(fieldName)
                        || DetailsViewDefaultFields.FIELD_TEST_RUN_RUN_BY.equals(fieldName)
                        || DetailsViewDefaultFields.FIELD_DETECTEDBY.equals(fieldName)) {
                    fieldValue = Util.getUiDataFromModel(entityModelWrapper.getValue(fieldName),
                            DetailsViewDefaultFields.FIELD_FULL_NAME);
                } else {
                    fieldValue = Util.getUiDataFromModel(entityModelWrapper.getValue(fieldName));
                }

                JXLabel fieldValueLabel = new JXLabel();
                fieldValueLabel.setFont(new Font("Arial", Font.PLAIN, 12));
                fieldValueLabel.setText(fieldValue);
                fieldValueLabel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
                fieldValueLabel.setToolTipText(fieldValue);
                fieldValueLabel.setLineWrap(false);
                GridBagConstraints gbc2 = new GridBagConstraints();
                gbc2.insets = new Insets(10, 10, 0, 5);
                gbc2.anchor = GridBagConstraints.FIRST_LINE_START;
                gbc2.fill = GridBagConstraints.BOTH;
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
            emptyLabel.setFont(new Font("Arial", Font.BOLD, 12));
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.insets = new Insets(20, 10, 0, 5);
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
