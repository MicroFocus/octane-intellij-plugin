package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.openapi.ui.JBCheckboxMenuItem;
import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXButton;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Set;

/**
 * Created by manasse on 11/8/2017.
 */
public class FieldsSelectPopup extends JPopupMenu {

    private Set<String> defaultFields;
    private Set<String> selectedFields;
    private Set<String> allFields;

    private JScrollPane fieldsPanel;
    private JPanel fieldsRootPanel;

    public FieldsSelectPopup(Set<String> defaultFields, Set<String> allFields, Set<String> selectedFields, JComponent invoker) {
        this.defaultFields = defaultFields;
        this.allFields = allFields;
        this.selectedFields = selectedFields;

        setInvoker(invoker);
        setLayout(new BorderLayout());

        fieldsRootPanel = new JPanel();
        fieldsRootPanel.setBorder(new MatteBorder(2, 2, 2, 2, JBColor.border()));
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0,0};
        gbl.rowHeights = new int[]{0};
        gbl.columnWeights = new double[]{0.5, 0.3, 0.0};
        gbl.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0};
        fieldsRootPanel.setLayout(gbl);

        fieldsPanel = createFieldsPanel(selectedFields, allFields);

        JXTextField searchField = new JXTextField("Search fields  ");
        searchField.setColumns(15);
        searchField.setBorder(new MatteBorder(1, 1, 1, 1, JBColor.border()));
        GridBagConstraints gbcSearchField = new GridBagConstraints();
        gbcSearchField.insets = new Insets(10, 10, 10, 10);
        gbcSearchField.anchor = GridBagConstraints.NORTH;
        gbcSearchField.gridx = 0;
        gbcSearchField.gridy = 0;
        fieldsRootPanel.add(searchField, gbcSearchField);


        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        fieldsRootPanel.add(fieldsPanel, gbc1);

        JXButton resetButton = new JXButton("Reset");
        resetButton.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                fieldsPanel = createFieldsPanel(defaultFields, allFields);
            }

            @Override
            public void mousePressed(MouseEvent e) {
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }
        });
        GridBagConstraints gbcButton = new GridBagConstraints();
        gbcButton.insets = new Insets(10, 10, 10, 10);
        gbcButton.anchor = GridBagConstraints.NORTH;
        gbcButton.gridx = 0;
        gbcButton.gridy = 2;
        fieldsRootPanel.add(resetButton, gbcButton);
        add(fieldsRootPanel);

    }

    public JScrollPane createFieldsPanel(Set<String> selectedFields,Set<String> allFields){
        JPanel fields = new JPanel();
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0};
        gbl.rowHeights = new int[]{0};
        gbl.columnWeights = new double[]{0.0, 0.0, 0.0};
        gbl.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0};
        fields.setLayout(gbl);
        createFieldsList(selectedFields,allFields,fields);
        JScrollPane scrollPane = new JScrollPane(fields,JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension((int) fields.getPreferredSize().getWidth(),(int) fields.getPreferredSize().getHeight()/6));
        return scrollPane;
    }

    public void createFieldsList(Set<String> selectedfields,Set<String> allFields, JPanel panel) {
        int fieldsCount = 0;
        for (String field : allFields) {
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridx = 0;
            gbc.gridy = fieldsCount++;
            CheckBoxMenuItem menuItem = new CheckBoxMenuItem(prettifyLabels(field));
            menuItem.putClientProperty("doNotCancelPopup",new JComboBox().getClientProperty("doNotCancelPopup"));
            if(selectedfields.contains(field)){
                menuItem.setState(true);
            }
            panel.add(menuItem, gbc);
        }
    }

    public Set<String> getDefaultFields(){
        return defaultFields;
    }

    public Set<String> getSelectedFields(){
        return selectedFields;
    }


    private String prettifyLabels(String str1) {
        //for udfs
        str1 = str1.replaceAll("_udf", "");
        str1 = str1.replaceAll("_", " ");
        char[] chars = str1.toCharArray();
        chars[0] = Character.toUpperCase(chars[0]);
        for (int x = 1; x < chars.length; x++) {
            if (chars[x - 1] == ' ') {
                chars[x] = Character.toUpperCase(chars[x]);
            }
        }
        return new String(chars);
    }


    /**
     * Class for checkboxmenu item needed to override basic behaviour
     */
    class CheckBoxMenuItem extends JBCheckboxMenuItem {
        public CheckBoxMenuItem(String text){
            super(text);
        }

        @Override
        protected void processMouseEvent(MouseEvent evt){
            switch ( evt.getID()){
                case MouseEvent.MOUSE_ENTERED: break;
                case MouseEvent.MOUSE_CLICKED: doClick();
            }
        }
    }

}
