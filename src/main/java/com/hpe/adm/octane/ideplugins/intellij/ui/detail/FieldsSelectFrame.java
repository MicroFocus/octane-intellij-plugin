package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.DefaultEntityFieldsUtil;
import com.intellij.openapi.ui.JBCheckboxMenuItem;
import com.intellij.openapi.util.IconLoader;
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
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by manasse on 11/8/2017.
 */
public class FieldsSelectFrame extends JFrame {

    public interface SelectionListener extends EventListener {
        void valueChanged(SelectionEvent e);
    }

    public class SelectionEvent extends EventObject {
        public SelectionEvent(Object source) {
            super(source);
        }
    }

    private Set<String> defaultFields;
    private Map<Entity, Set<String>> selectedFieldsMap;
    private Set<String> selectedFields;
    private Set<String> allFields;
    private Map<String, String> prettyFields = new HashMap<>();
    private List<JCheckBoxMenuItem> menuItems;

    private boolean buttonPressed = false;
    private Entity entityType;
    private IdePluginPersistentState idePluginPersistentState;
    private JScrollPane fieldsScrollPanel;
    private JPanel fieldsRootPanel;
    private JXTextField searchField;
    private JButton fieldsLabel;
    private JPanel fieldsPanel;

    private List<SelectionListener> listeners = new ArrayList<>();

    public FieldsSelectFrame(Set<String> defaultFields, Set<String> allFields, Map<Entity, Set<String>> selectedFieldsMap, Entity entityType, IdePluginPersistentState idePluginPersistentState, JButton fieldsButton) {
        this.defaultFields = defaultFields;
        this.allFields = allFields;
        this.selectedFieldsMap = selectedFieldsMap;
        this.selectedFields = selectedFieldsMap.get(entityType);
        this.idePluginPersistentState = idePluginPersistentState;
        this.fieldsLabel = fieldsButton;
        this.entityType = entityType;

        fieldsButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (buttonPressed) {
                    setVisible(false);
                    buttonPressed = false;
                } else {
                    setLocation((int) fieldsButton.getLocationOnScreen().x - getWidth(), (int) fieldsButton.getLocationOnScreen().y+ 15);
                    setVisible(true);
                    buttonPressed = true;
                }
            }
        });


        if (defaultFields.containsAll(selectedFields) && selectedFields.containsAll(defaultFields)) {
            fieldsButton.setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT));
            fieldsButton.repaint();
        } else {
            fieldsButton.setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_NON_DEFAULT));
            fieldsButton.repaint();
        }

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
        searchField.getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                Set<String> searchfields = new HashSet<>();
                if (!searchField.getText().equals("")) {
                    for (String string : prettyFields.keySet().stream().filter(pf -> pf.contains(searchField.getText())).collect(Collectors.toSet())) {
                        searchfields.add(prettyFields.get(string));
                    }
                } else {
                    searchfields = allFields;
                }
                updateFieldsPanel(selectedFields, searchfields);
                fieldsPanel.repaint();
                revalidate();
                repaint();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                Set<String> searchfields = new HashSet<>();
                if (!searchField.getText().equals("")) {
                    for (String string : prettyFields.keySet().stream().filter(pf -> pf.contains(searchField.getText())).collect(Collectors.toSet())) {
                        searchfields.add(prettyFields.get(string));
                    }
                } else {
                    searchfields = allFields;
                }
                updateFieldsPanel(selectedFields, searchfields);
                fieldsPanel.repaint();
                revalidate();
                repaint();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                Set<String> searchfields = new HashSet<>();
                if (!searchField.getText().equals("")) {
                    for (String string : prettyFields.keySet().stream().filter(pf -> pf.contains(searchField.getText())).collect(Collectors.toSet())) {
                        searchfields.add(prettyFields.get(string));
                    }
                } else {
                    searchfields = allFields;
                }
                updateFieldsPanel(selectedFields, searchfields);
                fieldsPanel.repaint();
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

        createCheckBoxMenuItems(selectedFields, allFields);
        fieldsScrollPanel = createFieldsPanel(selectedFields, allFields);
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        fieldsRootPanel.add(fieldsScrollPanel, gbc1);

        JXButton resetButton = new JXButton("Reset");
        resetButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setSelectedFields(getDefaultFields());
                updateFieldsPanel(getSelectedFields(), allFields);
                fieldsPanel.repaint();
                revalidate();
                repaint();
                listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                selectedFieldsMap.replace(entityType, selectedFields);
                idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
                fieldsButton.setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT));
                fieldsButton.repaint();
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

            }

            @Override
            public void windowLostFocus(WindowEvent e) {
                setVisible(false);
                int mouseLocationX = MouseInfo.getPointerInfo().getLocation().x;
                int mouseLocationY = MouseInfo.getPointerInfo().getLocation().y;
                if ((mouseLocationX < fieldsButton.getLocationOnScreen().x) ||
                        (mouseLocationX > (fieldsButton.getLocationOnScreen().x + fieldsButton.getWidth())) ||
                        (mouseLocationY < fieldsButton.getLocationOnScreen().y) ||
                        (mouseLocationY > (fieldsButton.getLocationOnScreen().y + fieldsButton.getHeight()))) {
                    buttonPressed = false;
                }
            }
        });
        setAlwaysOnTop(true);
        setUndecorated(true);
        pack();
    }

    public JScrollPane createFieldsPanel(Set<String> selectedFields, Set<String> allFields) {
        fieldsPanel = new JPanel();
        fieldsPanel.setLayout(new BoxLayout(fieldsPanel, BoxLayout.Y_AXIS));
        updateFieldsPanel(selectedFields, allFields);
        fieldsPanel.revalidate();
        JScrollPane scrollPane = new JScrollPane(fieldsPanel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension((int) searchField.getPreferredSize().getWidth(), 200));
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        return scrollPane;
    }

    public void createCheckBoxMenuItems(Set<String> selectedfields, Set<String> allFields) {
        menuItems = new ArrayList<>();
        for (String field : allFields) {
            JBCheckboxMenuItem menuItem = new JBCheckboxMenuItem(prettifyLabels(field));
            menuItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (menuItem.isSelected()) {
                        selectedfields.add(field);
                    } else {
                        selectedfields.remove(field);
                    }
                    listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                    if (defaultFields.containsAll(selectedfields) && selectedfields.containsAll(defaultFields)) {
                        fieldsLabel.setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_DEFAULT));
                        fieldsLabel.repaint();
                    } else {
                        fieldsLabel.setIcon(IconLoader.findIcon(Constants.IMG_FIELD_SELECTION_NON_DEFAULT));
                        fieldsLabel.repaint();
                    }
                    selectedFieldsMap.replace(entityType, selectedFields);
                    idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
                }
            });
            if (selectedfields.contains(field))
                menuItem.setState(true);
            menuItems.add(menuItem);
            prettyFields.put(prettifyLabels(field), field);
        }
    }

    public void updateFieldsPanel(Set<String> selectedFields, Set<String> allFields) {
        fieldsPanel.removeAll();
        for (JCheckBoxMenuItem checkBoxMenuItem : menuItems) {
            if (allFields.stream().filter(e -> checkBoxMenuItem.getText().equals(prettifyLabels(e))).count() == 1) {
                if (selectedFields.stream().filter(e -> checkBoxMenuItem.getText().equals(prettifyLabels(e))).count() == 1) {
                    checkBoxMenuItem.setState(true);
                } else {
                    checkBoxMenuItem.setState(false);
                }
                fieldsPanel.add(checkBoxMenuItem);
            }
        }
        fieldsPanel.add(Box.createRigidArea(new Dimension((int) searchField.getPreferredSize().getWidth(), 200)));
    }

    public void addSelectionListener(SelectionListener selectionListener) {
        listeners.add(selectionListener);
    }

    public Set<String> getDefaultFields() {
        return defaultFields;
    }

    public Set<String> getSelectedFields() {
        return selectedFields;
    }

    private void setSelectedFields(Set<String> fields) {
        selectedFields.removeAll(selectedFields);
        selectedFields.addAll(fields);
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

}
