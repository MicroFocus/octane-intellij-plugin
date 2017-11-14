package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.DefaultEntityFieldsUtil;
import com.intellij.openapi.ui.JBCheckboxMenuItem;
import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXButton;
import org.jdesktop.swingx.JXTextField;
import org.json.JSONObject;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.*;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by manasse on 11/8/2017.
 */
public class FieldsSelectFrame extends JFrame {

    private Set<String> defaultFields;
    private Map<Entity,Set<String>> selectedFieldsMap;
    private Set<String> selectedFields;
    private Set<String> allFields;

    private boolean exitedFocus = false;
    private boolean visible = false;
    private IdePluginPersistentState idePluginPersistentState;
    private JScrollPane fieldsPanel;
    private JPanel fieldsRootPanel;
    private JXTextField searchField;

    public FieldsSelectFrame(Set<String> defaultFields, Set<String> allFields, Map<Entity,Set<String>> selectedFieldsMap, Entity entityType, IdePluginPersistentState idePluginPersistentState) {
        this.defaultFields = defaultFields;
        this.allFields = allFields;
        this.selectedFieldsMap = selectedFieldsMap;
        this.selectedFields = selectedFieldsMap.get(entityType);
        this.idePluginPersistentState = idePluginPersistentState;


        setLayout(new BorderLayout());
        fieldsRootPanel = new JPanel();
        fieldsRootPanel.setBorder(new MatteBorder(2, 2, 2, 2, JBColor.border()));
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0, 0};
        gbl.rowHeights = new int[]{0};
        gbl.columnWeights = new double[]{0.5, 0.3, 0.0};
        gbl.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0};
        fieldsRootPanel.setLayout(gbl);



        searchField = new JXTextField("Search fields  ");
        searchField.setColumns(15);
        searchField.setBorder(new MatteBorder(1, 1, 1, 1, JBColor.border()));
        searchField.getDocument().addDocumentListener(new DocumentListener(){

            @Override
            public void insertUpdate(DocumentEvent e) {
                Set<String> searchfields;
                if(!searchField.getText().equals("")){
                    searchfields = allFields.stream().filter(s -> s.contains(searchField.getText())).collect(Collectors.toSet());

                } else {
                    searchfields = allFields;
                }
                fieldsRootPanel.remove(fieldsPanel);
                fieldsPanel = createFieldsPanel(getSelectedFields(),searchfields);
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.gridx = 0;
                gbc1.gridy = 1;
                fieldsRootPanel.add(fieldsPanel,gbc1);
                fieldsRootPanel.repaint();
                revalidate();
                repaint();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                Set<String> searchfields;
                if(!searchField.getText().equals("")){
                    searchfields = allFields.stream().filter(s -> s.contains(searchField.getText())).collect(Collectors.toSet());

                } else {
                    searchfields = allFields;
                }
                fieldsRootPanel.remove(fieldsPanel);
                fieldsPanel = createFieldsPanel(getSelectedFields(),searchfields);
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.gridx = 0;
                gbc1.gridy = 1;
                fieldsRootPanel.add(fieldsPanel,gbc1);
                fieldsRootPanel.repaint();
                revalidate();
                repaint();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                Set<String> searchfields;
                if(!searchField.getText().equals("")){
                    searchfields = allFields.stream().filter(s -> s.contains(searchField.getText())).collect(Collectors.toSet());

                } else {
                    searchfields = allFields;
                }
                fieldsRootPanel.remove(fieldsPanel);
                fieldsPanel = createFieldsPanel(getSelectedFields(),searchfields);
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.gridx = 0;
                gbc1.gridy = 1;
                fieldsRootPanel.add(fieldsPanel,gbc1);
                fieldsRootPanel.repaint();
                revalidate();
                repaint();
            }
        });




        GridBagConstraints gbcSearchField = new GridBagConstraints();
        gbcSearchField.insets = new Insets(10, 10, 10, 10);
        gbcSearchField.anchor = GridBagConstraints.NORTH;
        gbcSearchField.gridx = 0;
        gbcSearchField.gridy = 0;
        fieldsRootPanel.add(searchField, gbcSearchField);

        fieldsPanel = createFieldsPanel(selectedFields, allFields);
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        fieldsRootPanel.add(fieldsPanel, gbc1);

        JXButton resetButton = new JXButton("Reset");
        resetButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setSelectedFields(getDefaultFields());
                fieldsRootPanel.remove(fieldsPanel);
                fieldsPanel = createFieldsPanel(getSelectedFields(),allFields);
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.gridx = 0;
                gbc1.gridy = 1;
                fieldsRootPanel.add(fieldsPanel,gbc1);
                fieldsRootPanel.repaint();
                revalidate();
                repaint();

                idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
            }
        });
        GridBagConstraints gbcButton = new GridBagConstraints();
        gbcButton.insets = new Insets(10, 10, 10, 10);
        gbcButton.anchor = GridBagConstraints.NORTH;
        gbcButton.gridx = 0;
        gbcButton.gridy = 2;
        fieldsRootPanel.add(resetButton, gbcButton);
        setContentPane(fieldsRootPanel);
        addWindowFocusListener(new WindowFocusListener() {
            @Override
            public void windowGainedFocus(WindowEvent e) {
                visible = true;
            }

            @Override
            public void windowLostFocus(WindowEvent e) {
                setVisible(false);
                exitedFocus = true;

            }
        });
        setAlwaysOnTop(true);
        setUndecorated(true);
        pack();
    }

    public void setVisible() {
        if(visible){
            setVisible(!visible);
            visible = false;
        }
        else {
            setVisible(!visible);
            visible = true;
        }
    }

    public JScrollPane createFieldsPanel(Set<String> selectedFields, Set<String> allFields) {
        JPanel fields = new JPanel();
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0};
        gbl.rowHeights = new int[]{0};
        gbl.columnWeights = new double[]{0.0, 0.0, 0.0};
        gbl.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0};
        fields.setLayout(gbl);
        createFieldsList(selectedFields, allFields, fields);
        JScrollPane scrollPane = new JScrollPane(fields, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension((int) searchField.getPreferredSize().getWidth(), 200));
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        return scrollPane;
    }

    public void createFieldsList(Set<String> selectedfields, Set<String> allFields, JPanel panel) {
        int fieldsCount = 0;
        for (String field : allFields) {
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.anchor = GridBagConstraints.NORTH;
            gbc.weighty = 1;
            gbc.anchor = GridBagConstraints.NORTH;
            gbc.gridx = 0;
            gbc.gridy = fieldsCount++;
            JBCheckboxMenuItem menuItem = new JBCheckboxMenuItem(prettifyLabels(field));
            menuItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if(selectedFields.contains(field)){
                        selectedFields.remove(field);
                    } else {
                        selectedFields.add(field);
                    }
                    idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
                }
            });
            if (selectedfields.contains(field)) {
                menuItem.setState(true);
            }
            panel.add(menuItem, gbc);
        }
    }

    public Set<String> getDefaultFields() {
        return defaultFields;
    }

    public Set<String> getSelectedFields() {
        return selectedFields;
    }

    private void setSelectedFields(Set<String> selectedFields) {
         this.selectedFields = selectedFields;
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
        public CheckBoxMenuItem(String text) {
            super(text);
        }

        @Override
        protected void processMouseEvent(MouseEvent evt) {
            switch (evt.getID()) {
                case MouseEvent.MOUSE_ENTERED:
                    break;
                case MouseEvent.MOUSE_CLICKED:
                    doClick();
            }
        }
    }

}
