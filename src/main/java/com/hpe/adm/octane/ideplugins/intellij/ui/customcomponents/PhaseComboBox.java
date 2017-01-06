package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.intellij.ui.JBColor;

import javax.swing.*;
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

    private class PhaseItemEditor extends BasicComboBoxEditor {
        private JPanel panel = new JPanel();
        private JLabel labelItem = new JLabel();
        private EntityModel selectedValue;

        public PhaseItemEditor() {
            panel.setLayout(new GridBagLayout());
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.fill = GridBagConstraints.HORIZONTAL;
            constraints.weightx = 1.0;
            constraints.insets = new Insets(2, 5, 2, 2);

            labelItem.setOpaque(false);
            labelItem.setHorizontalAlignment(JLabel.LEFT);
            labelItem.setForeground(JBColor.foreground());

            panel.add(labelItem, constraints);
            panel.setBackground(JBColor.background());
        }

        public Component getEditorComponent() {
            return this.panel;
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
            labelItem.setText(UiUtil.getUiDataFromModel(phaseItem.getValue("target_phase"), "name"));
            labelItem.setForeground(JBColor.foreground());
            labelItem.setBackground(JBColor.background());
//            labelItem.setIcon(new ImageIcon(phaseItem[1]));
        }
    }

    private class PhaseItemRenderer extends JPanel implements ListCellRenderer {
        private JLabel labelItem = new JLabel();

        public PhaseItemRenderer() {
            setLayout(new GridBagLayout());
            GridBagConstraints constraints = new GridBagConstraints();
            constraints.fill = GridBagConstraints.HORIZONTAL;
            constraints.weightx = 1.0;
            constraints.insets = new Insets(2, 2, 2, 2);

            labelItem.setOpaque(true);
            labelItem.setHorizontalAlignment(JLabel.LEFT);

            add(labelItem, constraints);
            setBackground(JBColor.background());
        }

        @Override
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                      boolean cellHasFocus) {
            EntityModel phaseItem = (EntityModel) value;

            labelItem.setText(UiUtil.getUiDataFromModel(phaseItem.getValue("target_phase"), "name"));
//            labelItem.setIcon(new ImageIcon(countryItem[1]));

            if (isSelected) {
                labelItem.setBackground(JBColor.background());
                labelItem.setForeground(JBColor.foreground());
            } else {
                labelItem.setForeground(JBColor.foreground());
                labelItem.setBackground(JBColor.background());
            }

            return this;
        }

    }
}
