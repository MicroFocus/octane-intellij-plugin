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
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.FieldMenuItem;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions.SelectFieldsAction;
import com.hpe.adm.octane.ideplugins.intellij.ui.listeners.SelectionEvent;
import com.hpe.adm.octane.ideplugins.intellij.ui.listeners.SelectionListener;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.FieldsUtil;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.openapi.ui.VerticalFlowLayout;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.JBUI;
import org.jdesktop.swingx.JXButton;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.util.List;
import java.util.*;
import java.util.stream.Collectors;


public class FieldsSelectPopup extends JFrame {

    private Collection<FieldMetadata> allFields;
    private Map<Entity, Set<String>> defaultFieldsMap;
    private Map<Entity, Set<String>> selectedFieldsMap;
    private Map<String, String> prettyFields = new HashMap<>();
    private List<FieldMenuItem> menuItems;

    @Inject
    private FieldsUtil fieldsUtil;

    private JXTextField searchField;
    private SelectFieldsAction selectFieldsAction;
    private JPanel fieldsPanel;
    private JXButton resetButton;
    private JXButton selectNoneButton;
    private JXButton selectAllButton;
    private EntityModelWrapper entityModelWrapper;
    private Entity entityType;

    private Set<SelectionListener> listeners = new HashSet<>();

    public FieldsSelectPopup() {

        setLayout(new BorderLayout());
        JPanel fieldsRootPanel = new JPanel();
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
        selectNoneButton.addActionListener(e -> {
            clearSearchField();
            noneButtonClicked();
        });

        selectAllButton = new JXButton("All");
        selectAllButton.setPreferredSize(selectNoneButton.getPreferredSize());
        selectAllButton.addActionListener(e -> {
            clearSearchField();
            allButtonClicked();
        });

        resetButton = new JXButton("Reset");
        resetButton.addActionListener(e -> {
            clearSearchField();
            resetButtonClicked();
        });

        buttonsPanel.add(selectAllButton);
        buttonsPanel.add(selectNoneButton);
        buttonsPanel.add(resetButton);

        GridBagConstraints gbcButton = new GridBagConstraints();
        gbcButton.insets = JBUI.insets(10);
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
        gbcSearchField.insets = JBUI.insets(10);
        gbcSearchField.anchor = GridBagConstraints.NORTH;
        gbcSearchField.gridx = 0;
        gbcSearchField.gridy = 0;
        fieldsRootPanel.add(searchField, gbcSearchField);

        JScrollPane fieldsScrollPanel = createFieldsPanel();
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
                menuItems.clear();
                allFields.clear();
                listeners.clear();
                dispose();
            }
        });

        setAlwaysOnTop(true);
        setUndecorated(true);
        pack();
    }

    private void setupPopupButtonState() {
        if (fieldsUtil.isDefaultState(entityType)) {
            selectFieldsAction.setDefaultFieldsIcon(true);
            resetButton.setEnabled(false);
            selectAllButton.setEnabled(true);
            selectNoneButton.setEnabled(true);
        } else {
            selectFieldsAction.setDefaultFieldsIcon(false);
            resetButton.setEnabled(true);
            if (selectedFieldsMap.get(entityType).size() == 0) {
                selectNoneButton.setEnabled(false);
                selectAllButton.setEnabled(true);
            } else if (selectedFieldsMap.get(entityType).size() == allFields.size()) {
                selectAllButton.setEnabled(false);
                selectNoneButton.setEnabled(true);
            }
        }
    }


    public void setEntityDetails(EntityModelWrapper entityModelWrapper, Entity entityType, Collection<FieldMetadata> allFields, SelectFieldsAction selectFieldsAction) {
        this.selectFieldsAction = selectFieldsAction;

        this.entityType = entityType;

        this.allFields = allFields.stream()
                .filter(e -> !Arrays.asList("phase", "name", "subtype", "rank", "id").contains(e.getName()))
                .collect(Collectors.toList());

        this.entityModelWrapper = entityModelWrapper;
        defaultFieldsMap = fieldsUtil.retrieveDefaultFields();
        selectedFieldsMap = fieldsUtil.retrieveSelectedFieldsFromPersistentState();
        setupPopupButtonState();
        createCheckBoxMenuItems();
        //populate the popup with the selected fields
        updateFieldsPanel(selectedFieldsMap.get(entityType), this.allFields);
    }

    private void clearSearchField() {
        searchField.setText("");
    }

    private void noneButtonClicked() {
        selectedFieldsMap.get(entityType).removeAll(selectedFieldsMap.get(entityType));
        updateFieldsPanel(getSelectedFields(), allFields);
        fieldsPanel.repaint();
        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
        selectedFieldsMap.replace(entityType, selectedFieldsMap.get(entityType));
        fieldsUtil.saveSelectedFields(selectedFieldsMap);
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
        selectedFieldsMap.replace(entityType, selectedFieldsMap.get(entityType));
        fieldsUtil.saveSelectedFields(selectedFieldsMap);
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
        selectedFieldsMap.replace(entityType, selectedFieldsMap.get(entityType));
        fieldsUtil.saveSelectedFields(selectedFieldsMap);
        selectFieldsAction.setDefaultFieldsIcon(true);
        resetButton.transferFocusUpCycle();
        resetButton.setEnabled(false);
        selectNoneButton.setEnabled(true);
        selectAllButton.setEnabled(true);
    }

    private void searchFieldAction() {
        Collection<FieldMetadata> searchfields;
        if (!searchField.getText().equals("")) {
            searchfields = allFields.stream()
                    .filter(pf -> pf.getLabel().toLowerCase().contains(searchField.getText().toLowerCase())).collect(Collectors.toSet());
        } else {
            searchfields = allFields;
        }
        if (searchfields.size() != 0) {
            updateFieldsPanel(selectedFieldsMap.get(entityType), searchfields);
        } else {
            createNoResultsPanel();
        }
        fieldsPanel.repaint();
        revalidate();
        repaint();
    }

    private JScrollPane createFieldsPanel() {
        fieldsPanel = new JPanel();
        fieldsPanel.setLayout(new VerticalFlowLayout());
        JScrollPane scrollPane = new JBScrollPane(fieldsPanel, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension((int) searchField.getPreferredSize().getWidth(), 200));
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        return scrollPane;
    }

    private void createCheckBoxMenuItems() {
        menuItems = new ArrayList<>();
        for (FieldMetadata fieldMetadata : allFields) {
            FieldMenuItem menuItem = new FieldMenuItem(fieldMetadata.getLabel());
            menuItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {

                    if (menuItem.isSelected()) {
                        selectedFieldsMap.get(entityType).add(fieldMetadata.getName());
                    } else {
                        selectedFieldsMap.get(entityType).remove(fieldMetadata.getName());
                    }
                    listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));

                    if (defaultFieldsMap.get(entityType).containsAll(selectedFieldsMap.get(entityType)) && selectedFieldsMap.get(entityType).containsAll(defaultFieldsMap.get(entityType))) {
                        selectFieldsAction.setDefaultFieldsIcon(true);
                        resetButton.setEnabled(false);
                        selectNoneButton.setEnabled(true);
                        selectAllButton.setEnabled(true);
                    } else {
                        selectFieldsAction.setDefaultFieldsIcon(false);
                        resetButton.setEnabled(true);

                        if (selectedFieldsMap.get(entityType).size() == 0) {
                            selectNoneButton.setEnabled(false);
                        } else {
                            selectNoneButton.setEnabled(true);
                        }

                        if (selectedFieldsMap.get(entityType).containsAll(allFields.stream().map(FieldMetadata::getName).collect(Collectors.toSet()))) {
                            selectAllButton.setEnabled(false);
                        } else {
                            selectAllButton.setEnabled(true);
                        }
                    }
                    selectedFieldsMap.replace(entityType, selectedFieldsMap.get(entityType));
                    fieldsUtil.saveSelectedFields(selectedFieldsMap);
                }
            });
            if (selectedFieldsMap.get(entityType).contains(fieldMetadata)) {
                menuItem.setState(true);
            }
            menuItems.add(menuItem);
            prettyFields.put(fieldMetadata.getLabel(), fieldMetadata.getName());
        }
    }

    private void updateFieldsPanel(Set<String> selectedFields, Collection<FieldMetadata> allFieldNames) {
        fieldsPanel.removeAll();
        for (FieldMenuItem checkBoxMenuItem : menuItems) {

            if (allFieldNames.stream().filter(e -> e.getLabel().equals(checkBoxMenuItem.getText())).count() == 1) {
                if (selectedFields.contains(prettyFields.get(checkBoxMenuItem.getText()))) {
                    checkBoxMenuItem.setState(true);
                } else {
                    checkBoxMenuItem.setState(false);
                }
                fieldsPanel.add(checkBoxMenuItem);
            }
        }
    }

    private void createNoResultsPanel() {
        fieldsPanel.removeAll();
        JMenuItem noResults = new JMenuItem("No results");
        noResults.setForeground(JBColor.RED);
        fieldsPanel.add(noResults, BorderLayout.EAST);
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
        return defaultFieldsMap.get(entityType);
    }

    /**
     * Gets the selected fields from the popup
     *
     * @return the selected fields
     */
    public Set<String> getSelectedFields() {
        return selectedFieldsMap.get(entityType);
    }

    private void setSelectedFields(Set<String> fields) {
        selectedFieldsMap.get(entityType).removeAll(selectedFieldsMap.get(entityType));
        selectedFieldsMap.get(entityType).addAll(fields);
    }

}
