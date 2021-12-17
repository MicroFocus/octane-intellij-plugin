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
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.DateTimeFieldEditor;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.FieldEditor;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.FieldEditorFactory;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field.ReferenceFieldEditor;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.html.HtmlPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.html.SwingHtmlPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.ColumnLayoutManager;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.IdeBorderFactory;
import com.intellij.util.ui.JBUI;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.*;
import java.awt.*;
import java.util.List;
import java.util.*;
import java.util.function.Predicate;

@SuppressWarnings("serial")
public class EntityFieldsPanel extends JXPanel {

    private Collection<FieldMetadata> fields;

    @Inject
    private FieldEditorFactory fieldFactory;

    private JPanel leftFieldsPanel;
    private JPanel rightFieldsPanel;
    private JPanel memoFieldsPanel;

    @Inject
    public EntityFieldsPanel() {

        BorderLayout rootLayout = new BorderLayout();
        rootLayout.setVgap(5);
        rootLayout.setVgap(5);
        this.setLayout(rootLayout);

        JPanel fieldsPanel = new JPanel();

        leftFieldsPanel = new JPanel();
        leftFieldsPanel.setLayout(new GridBagLayout());

        rightFieldsPanel = new JPanel();
        rightFieldsPanel.setLayout(new GridBagLayout());

        fieldsPanel.setLayout(new ColumnLayoutManager());
        fieldsPanel.add(leftFieldsPanel);
        fieldsPanel.add(rightFieldsPanel);

        this.add(fieldsPanel, BorderLayout.CENTER);

        memoFieldsPanel = new JPanel();
        BoxLayout memoFieldsLayout = new BoxLayout(memoFieldsPanel, BoxLayout.Y_AXIS);
        memoFieldsPanel.setLayout(memoFieldsLayout);

        memoFieldsPanel.setBorder(BorderFactory.createEmptyBorder(0,5,5,5));
        this.add(memoFieldsPanel, BorderLayout.SOUTH);
    }

    public void setFieldMetadata(Collection<FieldMetadata> fields) {
        this.fields = fields;
    }

    public void setEntityModel(EntityModelWrapper entityModelWrapper, Set<String> fieldsToShow) {
        leftFieldsPanel.removeAll();
        rightFieldsPanel.removeAll();
        memoFieldsPanel.removeAll();

        List<FieldMetadata> simpleFields = getFieldMetadata(
                fieldsToShow,
                fieldMetadata -> fieldMetadata.getFieldType() != FieldMetadata.FieldType.Memo
        );

        List<FieldMetadata> memoFields = getFieldMetadata(
                fieldsToShow,
                fieldMetadata -> fieldMetadata.getFieldType() == FieldMetadata.FieldType.Memo
        );

        int index = 0;
        int rowIndexLeft = 0;
        int rowIndexRight = 0;
        for (FieldMetadata fieldMetadata : simpleFields) {
            if (index % 2 == 0) {
                addToFieldPanel(leftFieldsPanel, entityModelWrapper, fieldMetadata, rowIndexLeft);
                rowIndexLeft++;
            } else {
                addToFieldPanel(rightFieldsPanel, entityModelWrapper, fieldMetadata, rowIndexRight);
                rowIndexRight++;
            }
            index++;
        }

        if (simpleFields.size() % 2 != 0) {
            JPanel fillerPanel = new JPanel();
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.fill = GridBagConstraints.BOTH;
            gbc.gridx = 0;
            gbc.gridy = rowIndexRight;
            double minHeight = leftFieldsPanel.getComponents()[0].getPreferredSize().getHeight();
            gbc.insets = JBUI.insets(5 + (int) minHeight,5, 0, 0);
            rightFieldsPanel.add(fillerPanel, gbc);
        }

        for(FieldMetadata memoField : memoFields) {
            memoFieldsPanel.add(Box.createRigidArea(new Dimension(0, 5)));
            String memoFieldContent = Util.getUiDataFromModel(entityModelWrapper.getValue(memoField.getName()));
            memoFieldsPanel.add(createMemoFieldPanel(memoField.getLabel(), memoFieldContent));
        }

        repaint();
        revalidate();
    }

    private JPanel createMemoFieldPanel(String fieldLabelString, String fieldContent) {

        BorderLayout layout = new BorderLayout();
        layout.setVgap(5);
        JPanel memoFieldPanel = new JPanel(layout);
        memoFieldPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 0, 0));

        JXLabel fieldLabel = new JXLabel();
        Font font = new Font(fieldLabel.getFont().getFontName(), Font.BOLD, fieldLabel.getFont().getSize());
        fieldLabel.setFont(font);
        fieldLabel.setText(fieldLabelString);
        memoFieldPanel.add(fieldLabel, BorderLayout.NORTH);

        HtmlPanel htmlPanel;
        htmlPanel = new SwingHtmlPanel();
        htmlPanel.setHtmlContent(fieldContent);
        htmlPanel.setBorder(IdeBorderFactory.createBorder());
        memoFieldPanel.add(htmlPanel, BorderLayout.CENTER);

        return memoFieldPanel;
    }

    /*
        Preserves the iteration order of the input param
     */
    private List<FieldMetadata> getFieldMetadata(Collection<String> fieldsToShow, Predicate<FieldMetadata> filter) {
        List<FieldMetadata> result = new ArrayList<>();
        for (String fieldName : fieldsToShow) {
            FieldMetadata fieldMetadata = getFieldMetadata(fieldName);
            if (fieldMetadata != null && filter.test(fieldMetadata)) {
                result.add(fieldMetadata);
            }
        }
        return result;
    }

    private FieldMetadata getFieldMetadata(String fieldName) {
        if(fields == null) { //TODO: atoth, temporal coupling
            return null;
        }

        return fields.stream()
                .filter(fieldMetadata -> !Arrays.asList("phase", "name", "subtype", "rank").contains(fieldMetadata.getName()))
                .filter(fieldMetadata -> fieldMetadata.getName().equals(fieldName))
                .findAny()
                .orElse(null);
    }

    private void addToFieldPanel(JPanel parent, EntityModelWrapper entityModelWrapper, FieldMetadata fieldMetadata, int rowIndex) {

        JXLabel fieldLabel = new JXLabel();
        Font font = new Font(fieldLabel.getFont().getFontName(), Font.BOLD, fieldLabel.getFont().getSize());
        fieldLabel.setFont(font);
        fieldLabel.setText(fieldMetadata.getLabel());

        GridBagConstraints gbcLabel = new GridBagConstraints();
        gbcLabel.anchor = GridBagConstraints.WEST;
        gbcLabel.gridx = 0;
        gbcLabel.gridy = rowIndex;  // last row
        gbcLabel.insets = JBUI.insets(5, 5, 0, 0);

        FieldEditor fieldEditor = fieldFactory.createFieldEditor(entityModelWrapper, fieldMetadata.getName());
        GridBagConstraints gbcEditor = new GridBagConstraints();
        gbcEditor.fill = GridBagConstraints.HORIZONTAL;
        gbcEditor.gridx = 1;
        gbcEditor.gridy = rowIndex;
        gbcEditor.weightx = 1;
        gbcEditor.insets = JBUI.insets(5, 5, 0, 5);

        parent.add(fieldLabel, gbcLabel);
        parent.add(fieldEditor, gbcEditor);

        Component clearButton;

        if (fieldEditor instanceof ReferenceFieldEditor) {
            clearButton = ((ReferenceFieldEditor) fieldEditor).getClearButton();
        } else if (fieldEditor instanceof DateTimeFieldEditor) {
            clearButton = ((DateTimeFieldEditor) fieldEditor).getClearButton();
        } else {
            return;
        }

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 2;
        gbc.gridy = rowIndex;
        gbc.insets = JBUI.insets(5, 0, 0, 5);
        parent.add(clearButton, gbc);
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
