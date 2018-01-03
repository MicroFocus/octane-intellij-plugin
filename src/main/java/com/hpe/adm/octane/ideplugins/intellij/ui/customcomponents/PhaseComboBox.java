/*
 * © 2018 EntIT Software LLC, a Micro Focus company, L.P.
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

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicComboBoxEditor;
import java.awt.*;
import java.util.Collection;

public class PhaseComboBox extends JComboBox {
    private DefaultComboBoxModel model;

    public PhaseComboBox() {
        model = new DefaultComboBoxModel();
        setModel(model);
        setRenderer(new PhaseItemRenderer());
        setEditor(new PhaseItemEditor());
    }

    /**
     * Add an array items to this combo box. Each item is an array of two String
     * elements: - first element is country name. - second element is path of an
     * image file for country flag.
     *
     * @param items
     */
    public void addItems(Collection<EntityModel> items) {
        for (EntityModel anItem : items) {
            model.addElement(anItem);
        }
    }

    private static final Border labelBorder = BorderFactory.createEmptyBorder(5,5,5,5);

    private class PhaseItemEditor extends BasicComboBoxEditor {
        private JLabel labelItem = new JLabel();
        private EntityModel selectedValue;

        public PhaseItemEditor() {
            labelItem = new JLabel();
            labelItem.setBorder(labelBorder);
            labelItem.setHorizontalAlignment(JLabel.LEFT);
            labelItem.setVerticalAlignment(JLabel.CENTER);
        }

        public Component getEditorComponent() {
            return labelItem;
        }

        public Object getItem() {
            return this.selectedValue;
        }

        public void setItem(Object item) {
            if (item == null) {
                return;
            }
            EntityModel phaseItem = (EntityModel) item;
            selectedValue = phaseItem;
            labelItem.setText(Util.getUiDataFromModel(phaseItem.getValue("target_phase"), "name"));
        }
    }

    private class PhaseItemRenderer extends JPanel implements ListCellRenderer {

        @Override
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                      boolean cellHasFocus) {
            EntityModel phaseItem = (EntityModel) value;
            if (null == value) {
                phaseItem = new EntityModel("target_phase", "Loading phases...");
            }

            JLabel labelItem = new JLabel();
            labelItem.setOpaque(true);
            labelItem.setBorder(labelBorder);
            labelItem.setHorizontalAlignment(JLabel.LEFT);
            labelItem.setVerticalAlignment(JLabel.CENTER);
            labelItem.setText(Util.getUiDataFromModel(phaseItem.getValue("target_phase"), "name"));

            if (isSelected) {
                labelItem.setBackground(UIUtil.getListSelectionBackground());
                labelItem.setForeground(UIUtil.getListSelectionForeground());
            }

            return labelItem;
        }

    }
}
