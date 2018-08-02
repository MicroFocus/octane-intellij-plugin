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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.FieldMenuItem;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions.SelectFieldsAction;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.DefaultEntityFieldsUtil;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
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


public class FieldsSelectPopup extends JFrame {

    public interface SelectionListener extends EventListener {
        void valueChanged(SelectionEvent e);
    }

    public class SelectionEvent extends EventObject {
        public SelectionEvent(Object source) {
            super(source);
        }
    }

    private Collection<FieldMetadata> allFields;
    private Map<Entity, Set<String>> defaultFieldsMap;
    private Map<Entity, Set<String>> selectedFieldsMap;
    private Map<String, String> prettyFields = new HashMap<>();
    private List<FieldMenuItem> menuItems;

    @Inject
    private IdePluginPersistentState idePluginPersistentState;

    private JScrollPane fieldsScrollPanel;
    private JPanel fieldsRootPanel;
    private JXTextField searchField;
    private SelectFieldsAction selectFieldsAction;
    private JPanel fieldsPanel;
    private JXButton resetButton;
    private JXButton selectNoneButton;
    private JXButton selectAllButton;
    private EntityModelWrapper entityModelWrapper;

    private List<SelectionListener> listeners = new ArrayList<>();

    public FieldsSelectPopup() {

        setLayout(new BorderLayout());
        fieldsRootPanel = new JPanel();
        fieldsRootPanel.setBorder(new MatteBorder(2, 2, 2, 2, JBColor.border()));
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0, 0};
        gbl.rowHeights = new int[]{0};
        gbl.columnWeights = new double[]{0.5, 0.3, 0.0};
        gbl.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0};
        fieldsRootPanel.setLayout(gbl);


        JPanel buttonsPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setHgap(0);
        buttonsPanel.setLayout(flowLayout);


        selectNoneButton = new JXButton("None");
        selectNoneButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                clearSearchField();
                noneButtonClicked();
            }
        });

        selectAllButton = new JXButton("All");
        selectAllButton.setPreferredSize(selectNoneButton.getPreferredSize());
        selectAllButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                clearSearchField();
                allButtonClicked();
            }
        });

        resetButton = new JXButton("Reset");
        resetButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                clearSearchField();
                resetButtonClicked();
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

        searchField = new JXTextField("Search fields");
        searchField.setPreferredSize(new Dimension(buttonsPanel.getPreferredSize().width, searchField.getPreferredSize().height));
        searchField.setBorder(new MatteBorder(1, 1, 1, 1, JBColor.border()));
        searchField.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                searchFieldAction();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                searchFieldAction();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                searchFieldAction();
            }
        });

        GridBagConstraints gbcSearchField = new GridBagConstraints();
        gbcSearchField.insets = new Insets(10, 10, 10, 10);
        gbcSearchField.anchor = GridBagConstraints.NORTH;
        gbcSearchField.gridx = 0;
        gbcSearchField.gridy = 0;
        fieldsRootPanel.add(searchField, gbcSearchField);

        fieldsScrollPanel = createFieldsPanel();
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        fieldsRootPanel.add(fieldsScrollPanel, gbc1);


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

        setAlwaysOnTop(true);
        setUndecorated(true);
        pack();
    }

    private void setupPopupButtonState() {
        if (defaultFieldsMap.get(entityModelWrapper.getEntityType()).containsAll(selectedFieldsMap.get(entityModelWrapper.getEntityType()))
                && selectedFieldsMap.get(entityModelWrapper.getEntityType()).containsAll(defaultFieldsMap.get(entityModelWrapper.getEntityType()))) {
            selectFieldsAction.setDefaultFieldsIcon(true);
            resetButton.setEnabled(false);
            selectAllButton.setEnabled(true);
            selectNoneButton.setEnabled(true);
        } else {
            selectFieldsAction.setDefaultFieldsIcon(false);
            resetButton.setEnabled(true);
            if (selectedFieldsMap.get(entityModelWrapper.getEntityType()).size() == 0) {
                selectNoneButton.setEnabled(false);
                selectAllButton.setEnabled(true);
            } else if (selectedFieldsMap.get(entityModelWrapper.getEntityType()).size() == allFields.size()) {
                selectAllButton.setEnabled(false);
                selectNoneButton.setEnabled(true);
            }
        }
    }

    private void retrieveSelectedFieldsFromPersistentState() {
        defaultFieldsMap = DefaultEntityFieldsUtil.getDefaultFields();
        JSONObject selectedFieldsJson = idePluginPersistentState.loadState(IdePluginPersistentState.Key.SELECTED_FIELDS);
        if (selectedFieldsJson == null) {
            selectedFieldsMap = defaultFieldsMap;
        } else {
            selectedFieldsMap = DefaultEntityFieldsUtil.entityFieldsFromJson(selectedFieldsJson.toString());
            if (selectedFieldsMap == null) {
                selectedFieldsMap = defaultFieldsMap;
            }
        }
    }

    public void setEntityDetails(EntityModelWrapper entityModelWrapper, Collection<FieldMetadata> allFields, SelectFieldsAction selectFieldsAction) {
        this.selectFieldsAction = selectFieldsAction;
        this.allFields = allFields.stream()
                .filter(e -> !Arrays.asList("phase", "name", "subtype", "description").contains(e.getName()))
                .collect(Collectors.toList());
        this.entityModelWrapper = entityModelWrapper;
        retrieveSelectedFieldsFromPersistentState();
        setupPopupButtonState();
        createCheckBoxMenuItems();
        //populate the popup with the selected fields
        updateFieldsPanel(selectedFieldsMap.get(entityModelWrapper.getEntityType()), this.allFields);
    }

    private void clearSearchField() {
        searchField.setText("");
    }

    private void noneButtonClicked() {
        selectedFieldsMap.get(entityModelWrapper.getEntityType()).removeAll(selectedFieldsMap.get(entityModelWrapper.getEntityType()));
        updateFieldsPanel(getSelectedFields(), allFields);
        fieldsPanel.repaint();
        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
        selectedFieldsMap.replace(entityModelWrapper.getEntityType(), selectedFieldsMap.get(entityModelWrapper.getEntityType()));
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
        selectFieldsAction.setDefaultFieldsIcon(false);
        selectNoneButton.transferFocusUpCycle();
        resetButton.setEnabled(true);
        selectNoneButton.setEnabled(false);
        selectAllButton.setEnabled(true);
    }

    private void allButtonClicked() {
        setSelectedFields(allFields.stream().map(FieldMetadata::getName).collect(Collectors.toSet()));
        updateFieldsPanel(getSelectedFields(), allFields);
        fieldsPanel.repaint();
        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
        selectedFieldsMap.replace(entityModelWrapper.getEntityType(), selectedFieldsMap.get(entityModelWrapper.getEntityType()));
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
        selectFieldsAction.setDefaultFieldsIcon(false);
        selectAllButton.transferFocusUpCycle();
        resetButton.setEnabled(true);
        selectNoneButton.setEnabled(true);
        selectAllButton.setEnabled(false);
    }

    private void resetButtonClicked() {
        setSelectedFields(getDefaultFields());
        updateFieldsPanel(getSelectedFields(), allFields);
        fieldsPanel.repaint();
        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
        selectedFieldsMap.replace(entityModelWrapper.getEntityType(), selectedFieldsMap.get(entityModelWrapper.getEntityType()));
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
        selectFieldsAction.setDefaultFieldsIcon(true);
        resetButton.transferFocusUpCycle();
        resetButton.setEnabled(false);
        selectNoneButton.setEnabled(true);
        selectAllButton.setEnabled(true);
    }

    private void searchFieldAction() {
        Collection<FieldMetadata> searchfields = new HashSet<>();
        if (!searchField.getText().equals("")) {
            for (FieldMetadata fieldMetadata : allFields.stream()
                    .filter(pf -> pf.getLabel().toLowerCase().contains(searchField.getText().toLowerCase()))
                    .collect(Collectors.toSet())) {
                searchfields.add(fieldMetadata);
            }
        } else {
            searchfields = allFields;
        }
        if (searchfields.size() != 0) {
            updateFieldsPanel(selectedFieldsMap.get(entityModelWrapper.getEntityType()), searchfields);
        } else {
            createNoResultsPanel();
        }
        fieldsPanel.repaint();
        revalidate();
        repaint();
    }

    private JScrollPane createFieldsPanel() {
        fieldsPanel = new JPanel();
        fieldsPanel.setLayout(new BoxLayout(fieldsPanel, BoxLayout.Y_AXIS));
        fieldsPanel.revalidate();
        JScrollPane scrollPane = new JBScrollPane(fieldsPanel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension((int) searchField.getPreferredSize().getWidth(), 200));
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        return scrollPane;
    }

    private void createCheckBoxMenuItems() {
        menuItems = new ArrayList<>();
        for (FieldMetadata fieldMetadata : allFields) {
            if (!"description".equals(fieldMetadata.getName()) && !"phase".equals(fieldMetadata.getName())) {
                FieldMenuItem menuItem = new FieldMenuItem(fieldMetadata.getLabel());
                menuItem.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (menuItem.isSelected()) {
                            selectedFieldsMap.get(entityModelWrapper.getEntityType()).add(fieldMetadata.getName());
                        } else {
                            selectedFieldsMap.get(entityModelWrapper.getEntityType()).remove(fieldMetadata.getName());
                        }
                        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                        if (defaultFieldsMap.get(entityModelWrapper.getEntityType()).containsAll(selectedFieldsMap.get(entityModelWrapper.getEntityType())) && selectedFieldsMap.get(entityModelWrapper.getEntityType()).containsAll(defaultFieldsMap.get(entityModelWrapper.getEntityType()))) {
                            selectFieldsAction.setDefaultFieldsIcon(true);
                            resetButton.setEnabled(false);
                            selectNoneButton.setEnabled(true);
                            selectAllButton.setEnabled(true);
                        } else {
                            selectFieldsAction.setDefaultFieldsIcon(false);
                            resetButton.setEnabled(true);

                            if (selectedFieldsMap.get(entityModelWrapper.getEntityType()).size() == 0) {
                                selectNoneButton.setEnabled(false);
                            } else {
                                selectNoneButton.setEnabled(true);
                            }

                            if (selectedFieldsMap.get(entityModelWrapper.getEntityType()).containsAll(allFields.stream().map(FieldMetadata::getName).collect(Collectors.toSet()))) {
                                selectAllButton.setEnabled(false);
                            } else {
                                selectAllButton.setEnabled(true);
                            }
                        }
                        selectedFieldsMap.replace(entityModelWrapper.getEntityType(), selectedFieldsMap.get(entityModelWrapper.getEntityType()));
                        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));

                    }
                });
                if (selectedFieldsMap.get(entityModelWrapper.getEntityType()).contains(fieldMetadata)) {
                    menuItem.setState(true);
                }
                menuItems.add(menuItem);
                prettyFields.put(fieldMetadata.getLabel(), fieldMetadata.getName());
            }
        }
    }

    private void updateFieldsPanel(Set<String> selectedFields, Collection<FieldMetadata> allFieldNames) {
        fieldsPanel.removeAll();
        int rowCount = 0;
        for (FieldMenuItem checkBoxMenuItem : menuItems) {

            if (allFieldNames.stream().filter(e -> e.getLabel().equals(checkBoxMenuItem.getText())).count() == 1) {
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

    private void createNoResultsPanel() {
        fieldsPanel.removeAll();
        JMenuItem noResults = new JMenuItem("No results");
        noResults.setForeground(Color.RED);
        fieldsPanel.add(noResults, BorderLayout.EAST);
        fieldsPanel.add(Box.createRigidArea(new Dimension((int) searchField.getPreferredSize().getWidth(), 200 - (int) noResults.getPreferredSize().getHeight())));
    }

    /**
     * Adds a listener to the listeners of the class
     *
     * @param selectionListener custom listener to be added
     */
    public void addSelectionListener(SelectionListener selectionListener) {
        listeners.add(selectionListener);
    }

    /**
     * Gets the default fields
     *
     * @return the default fields of the entity opened in detailed view
     */
    public Set<String> getDefaultFields() {
        return defaultFieldsMap.get(entityModelWrapper.getEntityType());
    }

    /**
     * Gets the selected fields from the popup
     *
     * @return the selected fields
     */
    public Set<String> getSelectedFields() {
        return selectedFieldsMap.get(entityModelWrapper.getEntityType());
    }

    private void setSelectedFields(Set<String> fields) {
        selectedFieldsMap.get(entityModelWrapper.getEntityType()).removeAll(selectedFieldsMap.get(entityModelWrapper.getEntityType()));
        selectedFieldsMap.get(entityModelWrapper.getEntityType()).addAll(fields);
    }


    public void addPersistentStateListener() {
        idePluginPersistentState.addStateChangedHandler(new IdePluginPersistentState.SettingsChangedHandler() {
            @Override
            public void stateChanged(IdePluginPersistentState.Key key, JSONObject value) {
                if (key == IdePluginPersistentState.Key.SELECTED_FIELDS) {
                    retrieveSelectedFieldsFromPersistentState();
                    listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                    updateFieldsPanel(selectedFieldsMap.get(entityModelWrapper.getEntityType()), allFields);
                    setupPopupButtonState();
                }
            }
        });
    }
}
