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
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.DateTimeFieldEditor;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.FieldEditor;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.FieldEditorFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.ReferenceFieldEditor;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.html.HtmlPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.html.JavaFxHtmlPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.html.SwingHtmlPanel;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.util.ui.JBUI;
import org.jdesktop.swingx.JXPanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

@SuppressWarnings("serial")
public class EntityFieldsPanel extends JXPanel implements Scrollable {

    private Collection<FieldMetadata> fields;

    @Inject
    private FieldEditorFactory fieldFactory;

    @Inject
    public EntityFieldsPanel() {

        setBorder(BorderFactory.createLineBorder(Color.RED));

        GridBagLayout gbl = new GridBagLayout();
        //gbl.columnWidths = new int[]{0, 0, 0, 0, 0, 0};
        //gbl.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        setLayout(gbl);

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                super.componentResized(e);
                //gbl.columnWidths = new int[]{0, getWidth() / 2, 0, 0, getWidth() / 2, 0};
                repaint();
            }
        });
    }

    public void setFieldMetadata(Collection<FieldMetadata> fields) {
        this.fields = fields;
    }

    public void setEntityModel(EntityModelWrapper entityModelWrapper, Set<String> fieldsToShow) {
        removeAll();
        int columnCount = 0;
        int rowCount = 0;

        //This needs to be checked in case the user has memo fields selected from a previous version of the plugin
        Collection<FieldMetadata> possibleFields = fields.stream()
                .filter(e -> !Arrays.asList("phase", "name", "subtype", "description", "rank").contains(e.getName()))
                .filter(e -> e.getFieldType() != FieldMetadata.FieldType.Memo)
                .collect(Collectors.toList());

        for (String fieldName : fieldsToShow) {

            /*

            FieldMetadata fieldMetadata =
                    possibleFields
                            .stream()
                            .filter(currentFieldMetadata -> currentFieldMetadata.getName().equals(fieldName))
                            .findAny()
                            .orElse(null);

            if (fieldMetadata == null) {
                continue;
            }

            JXLabel fieldLabel = new JXLabel();
            Font font = new Font(fieldLabel.getFont().getFontName(), Font.BOLD, fieldLabel.getFont().getSize());
            fieldLabel.setFont(font);
            fieldLabel.setText(fieldMetadata.getLabel());
            GridBagConstraints gbc1 = new GridBagConstraints();
            gbc1.anchor = GridBagConstraints.WEST;
            gbc1.insets = JBUI.insets(0, 5, 10, 0);
            gbc1.gridx = columnCount++;
            gbc1.gridy = rowCount;

            FieldEditor fieldValueLabel = fieldFactory.createFieldEditor(entityModelWrapper, fieldName);
            GridBagConstraints gbc2 = new GridBagConstraints();
            gbc2.insets = JBUI.insets(0, 10, 10, 0);
            gbc2.anchor = GridBagConstraints.WEST;
            gbc2.fill = GridBagConstraints.HORIZONTAL;
            gbc2.gridx = columnCount++;
            gbc2.gridy = rowCount;
            gbc2.weightx = 0.5;

            add(fieldLabel, gbc1);
            add(fieldValueLabel, gbc2);
            addClearButton(fieldValueLabel, rowCount, columnCount);
            columnCount++;

            if (columnCount > 5) {
                rowCount++;
                columnCount = 0;
            }

            */
        }

        addDescription(entityModelWrapper, rowCount);

        repaint();
        revalidate();
    }

    private void addDescription(EntityModelWrapper entityModelWrapper, int rowCount) {

        String descriptionContent = Util.getUiDataFromModel(entityModelWrapper.getValue(DetailsViewDefaultFields.FIELD_DESCRIPTION));

        //descriptionPanel.setBorder(new TitledBorder(IdeBorderFactory.createBorder(), "Description"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = rowCount;
        gbc.gridwidth = 6;

        JPanel panel = new JPanel(new BorderLayout());

        panel.setBackground(Color.RED);

        add(panel, gbc);

        HtmlPanel descriptionPanel = new SwingHtmlPanel();
        descriptionPanel.setHtmlContent(descriptionContent);
        panel.add(descriptionPanel);
        //add(descriptionPanel, gbc);

        //Platform.runLater(() -> descriptionPanel.setHtmlContent(descriptionContent));
    }

    private void addClearButton(FieldEditor fieldEditor, int rowCount, int column) {
        Component clearButton;
        if (fieldEditor instanceof ReferenceFieldEditor) {
            clearButton = ((ReferenceFieldEditor) fieldEditor).getClearButton();
        } else if (fieldEditor instanceof DateTimeFieldEditor) {
            clearButton = ((DateTimeFieldEditor) fieldEditor).getClearButton();
        } else {
            return;
        }
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.insets = JBUI.insets(0, 5, 10, 5);
        gbc2.gridx = column;
        gbc2.gridy = rowCount;
        add(clearButton, gbc2);
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        return true;
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        return false;
    }
}
