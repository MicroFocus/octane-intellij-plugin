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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
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
    private Entity entityType;
    private IdePluginPersistentState idePluginPersistentState;
    private JScrollPane fieldsScrollPanel;
    private JPanel fieldsRootPanel;
    private JXTextField searchField;
    private EntityDetailPresenter.SelectFieldsAction fieldsActionButton;
    private JPanel fieldsPanel;
    private JXButton resetButton;

    private List<SelectionListener> listeners = new ArrayList<>();

    public FieldsSelectFrame(Set<String> defaultFields, Set<String> allFields, Map<Entity, Set<String>> selectedFieldsMap, Entity entityType, IdePluginPersistentState idePluginPersistentState, EntityDetailPresenter.SelectFieldsAction fieldsButton) {
        this.defaultFields = defaultFields;
        this.allFields = allFields;
        this.selectedFieldsMap = selectedFieldsMap;
        this.selectedFields = selectedFieldsMap.get(entityType);
        this.idePluginPersistentState = idePluginPersistentState;
        this.fieldsActionButton = fieldsButton;
        this.entityType = entityType;

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
                    for (String string : prettyFields.keySet().stream().filter(pf -> pf.toLowerCase().contains(searchField.getText().toLowerCase())).collect(Collectors.toSet())) {
                        searchfields.add(prettyFields.get(string));
                    }
                } else {
                    searchfields = allFields;
                }
                if (searchfields.size() != 0) {
                    updateFieldsPanel(selectedFields, searchfields);
                } else {
                    createNoResultsPanel();
                }
                fieldsPanel.repaint();
                revalidate();
                repaint();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                Set<String> searchfields = new HashSet<>();
                if (!searchField.getText().equals("")) {
                    for (String string : prettyFields.keySet().stream().filter(pf -> pf.toLowerCase().contains(searchField.getText().toLowerCase())).collect(Collectors.toSet())) {
                        searchfields.add(prettyFields.get(string));
                    }
                } else {
                    searchfields = allFields;
                }
                if (searchfields.size() != 0) {
                    updateFieldsPanel(selectedFields, searchfields);
                } else {
                    createNoResultsPanel();
                }
                fieldsPanel.repaint();
                revalidate();
                repaint();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                Set<String> searchfields = new HashSet<>();
                if (!searchField.getText().equals("")) {
                    for (String string : prettyFields.keySet().stream().filter(pf -> pf.toLowerCase().contains(searchField.getText().toLowerCase())).collect(Collectors.toSet())) {
                        searchfields.add(prettyFields.get(string));
                    }
                } else {
                    searchfields = allFields;
                }
                if (searchfields.size() != 0) {
                    updateFieldsPanel(selectedFields, searchfields);
                } else {
                    createNoResultsPanel();
                }
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

        createCheckBoxMenuItems();
        fieldsScrollPanel = createFieldsPanel();
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        fieldsRootPanel.add(fieldsScrollPanel, gbc1);

        JPanel buttonsPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setHgap(0);
        buttonsPanel.setLayout(flowLayout);

        JXButton selectNoneButton = new JXButton("None");
        selectNoneButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                selectedFields.removeAll(selectedFields);
                updateFieldsPanel(getSelectedFields(), allFields);
                fieldsPanel.repaint();
                listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                selectedFieldsMap.replace(entityType, selectedFields);
                idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
                fieldsActionButton.setDefaultFieldsIcon(false);
                selectNoneButton.transferFocusUpCycle();
                resetButton.setEnabled(true);
            }
        });

        JXButton selectAllButton = new JXButton("All");
        selectAllButton.setPreferredSize(selectNoneButton.getPreferredSize());
        selectAllButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setSelectedFields(allFields);
                updateFieldsPanel(getSelectedFields(), allFields);
                fieldsPanel.repaint();
                listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                selectedFieldsMap.replace(entityType, selectedFields);
                idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
                fieldsActionButton.setDefaultFieldsIcon(false);
                selectAllButton.transferFocusUpCycle();
                resetButton.setEnabled(true);
            }
        });

        resetButton = new JXButton("Reset");
        resetButton.setPreferredSize(selectNoneButton.getPreferredSize());
        resetButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setSelectedFields(getDefaultFields());
                updateFieldsPanel(getSelectedFields(), allFields);
                fieldsPanel.repaint();
                listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                selectedFieldsMap.replace(entityType, selectedFields);
                idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
                fieldsActionButton.setDefaultFieldsIcon(true);
                resetButton.transferFocusUpCycle();
                resetButton.setEnabled(false);

            }
        });

        buttonsPanel.add(selectAllButton);
        buttonsPanel.add(selectNoneButton);
        buttonsPanel.add(resetButton);

        GridBagConstraints gbcButton = new GridBagConstraints();
        gbcButton.insets = new Insets(10, 10, 10, 10);
        gbcButton.anchor = GridBagConstraints.NORTH;
        gbcButton.gridx = 0;
        gbcButton.gridy = 2;
        fieldsRootPanel.add(buttonsPanel, gbcButton);


        setContentPane(fieldsRootPanel);
        addWindowFocusListener(new WindowFocusListener() {
            @Override
            public void windowGainedFocus(WindowEvent e) {
            }

            @Override
            public void windowLostFocus(WindowEvent e) {
                setVisible(false);
            }
        });

        if (defaultFields.containsAll(selectedFields) && selectedFields.containsAll(defaultFields)) {
            fieldsActionButton.setDefaultFieldsIcon(true);
            resetButton.setEnabled(false);
        } else {
            fieldsActionButton.setDefaultFieldsIcon(false);
        }

        setAlwaysOnTop(true);
        setUndecorated(true);
        pack();
    }

    public JScrollPane createFieldsPanel() {
        fieldsPanel = new JPanel();
        fieldsPanel.setLayout(new BoxLayout(fieldsPanel, BoxLayout.Y_AXIS));
        updateFieldsPanel(selectedFields, allFields);
        fieldsPanel.revalidate();
        JScrollPane scrollPane = new JScrollPane(fieldsPanel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension((int) searchField.getPreferredSize().getWidth(), 200));
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        return scrollPane;
    }

    public void createCheckBoxMenuItems() {
        menuItems = new ArrayList<>();
        for (String field : allFields) {
            if (!"description".equals(field) && !"phase".equals(field)) {
                JBCheckboxMenuItem menuItem = new JBCheckboxMenuItem(prettifyLabels(field));
                menuItem.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (menuItem.isSelected()) {
                            selectedFields.add(field);
                        } else {
                            selectedFields.remove(field);
                        }
                        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                        if (defaultFields.containsAll(selectedFields) && selectedFields.containsAll(defaultFields)) {
                            fieldsActionButton.setDefaultFieldsIcon(true);
                            resetButton.setEnabled(false);
                        } else {
                            fieldsActionButton.setDefaultFieldsIcon(false);
                            resetButton.setEnabled(true);
                        }
                        selectedFieldsMap.replace(entityType, selectedFields);
                        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));

                    }
                });
                if (selectedFields.contains(field)) {
                    menuItem.setState(true);
                }
                menuItems.add(menuItem);
                prettyFields.put(prettifyLabels(field), field);
            }
        }
    }

    public void updateFieldsPanel(Set<String> selectedFields, Set<String> allFields) {
        fieldsPanel.removeAll();
        int rowCount = 0;
        for (JCheckBoxMenuItem checkBoxMenuItem : menuItems) {
            if (allFields.contains(prettyFields.get(checkBoxMenuItem.getText()))) {
                if (selectedFields.contains(prettyFields.get(checkBoxMenuItem.getText()))) {
                    checkBoxMenuItem.setState(true);
                } else {
                    checkBoxMenuItem.setState(false);
                }
                fieldsPanel.add(checkBoxMenuItem);
                rowCount++;
            }
        }
        fieldsPanel.add(Box.createRigidArea(new Dimension((int) searchField.getPreferredSize().getWidth(), rowCount > 9 ? 0 : (200 - 20 * rowCount))));
    }

    public void createNoResultsPanel() {
        fieldsPanel.removeAll();
        JMenuItem noResults = new JMenuItem("No results");
        noResults.setForeground(Color.RED);
        fieldsPanel.add(noResults, BorderLayout.EAST);
        fieldsPanel.add(Box.createRigidArea(new Dimension((int) searchField.getPreferredSize().getWidth(), 200 - (int) noResults.getPreferredSize().getHeight())));
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
