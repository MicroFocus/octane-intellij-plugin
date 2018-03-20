package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.FieldMenuItem;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.DefaultEntityFieldsUtil;
import com.intellij.openapi.ui.JBCheckboxMenuItem;
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
    private Collection<FieldMetadata> allFields;
    private Map<String, String> prettyFields = new HashMap<>();
    private List<FieldMenuItem> menuItems;
    private Entity entityType;
    private IdePluginPersistentState idePluginPersistentState;
    private JScrollPane fieldsScrollPanel;
    private JPanel fieldsRootPanel;
    private JXTextField searchField;
    private EntityDetailPresenter.SelectFieldsAction fieldsActionButton;
    private JPanel fieldsPanel;
    private JXButton resetButton;
    private JXButton selectNoneButton;
    private JXButton selectAllButton;

    private List<SelectionListener> listeners = new ArrayList<>();

    public FieldsSelectFrame(Set<String> defaultFields, Collection<FieldMetadata> allFields, Map<Entity, Set<String>> selectedFieldsMap, Entity entityType, IdePluginPersistentState idePluginPersistentState, EntityDetailPresenter.SelectFieldsAction fieldsButton) {
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


        JPanel buttonsPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setHgap(0);
        buttonsPanel.setLayout(flowLayout);


        selectNoneButton = new JXButton("None");
        selectNoneButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                noneButtonClicked();
            }
        });

        selectAllButton = new JXButton("All");
        selectAllButton.setPreferredSize(selectNoneButton.getPreferredSize());
        selectAllButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                allButtonClicked();
            }
        });

        resetButton = new JXButton("Reset");
        resetButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
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

        createCheckBoxMenuItems();
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

        if (defaultFields.containsAll(selectedFields) && selectedFields.containsAll(defaultFields)) {
            fieldsActionButton.setDefaultFieldsIcon(true);
            resetButton.setEnabled(false);
        } else {
            fieldsActionButton.setDefaultFieldsIcon(false);
            resetButton.setEnabled(true);
        }

        setAlwaysOnTop(true);
        setUndecorated(true);
        pack();
    }

    private void noneButtonClicked() {
        selectedFields.removeAll(selectedFields);
        updateFieldsPanel(getSelectedFields(), allFields);
        fieldsPanel.repaint();
        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
        selectedFieldsMap.replace(entityType, selectedFields);
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
        fieldsActionButton.setDefaultFieldsIcon(false);
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
        selectedFieldsMap.replace(entityType, selectedFields);
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
        fieldsActionButton.setDefaultFieldsIcon(false);
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
        selectedFieldsMap.replace(entityType, selectedFields);
        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));
        fieldsActionButton.setDefaultFieldsIcon(true);
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
            updateFieldsPanel(selectedFields, searchfields);
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
        updateFieldsPanel(selectedFields, allFields);
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
                //JBCheckboxMenuItem menuItem = new JBCheckboxMenuItem(fieldMetadata.getLabel());
                FieldMenuItem menuItem = new FieldMenuItem(fieldMetadata.getLabel());
                menuItem.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (menuItem.isSelected()) {
                            selectedFields.add(fieldMetadata.getName());
                        } else {
                            selectedFields.remove(fieldMetadata.getName());
                        }
                        listeners.forEach(listener -> listener.valueChanged(new SelectionEvent(this)));
                        if (defaultFields.containsAll(selectedFields) && selectedFields.containsAll(defaultFields)) {
                            fieldsActionButton.setDefaultFieldsIcon(true);
                            resetButton.setEnabled(false);
                            selectNoneButton.setEnabled(true);
                            selectAllButton.setEnabled(true);
                        } else {
                            fieldsActionButton.setDefaultFieldsIcon(false);
                            resetButton.setEnabled(true);

                            if (selectedFields.size() == 0) {
                                selectNoneButton.setEnabled(false);
                            } else {
                                selectNoneButton.setEnabled(true);
                            }

                            if (selectedFields.containsAll(allFields.stream().map(FieldMetadata::getName).collect(Collectors.toSet()))) {
                                selectAllButton.setEnabled(false);
                            } else {
                                selectAllButton.setEnabled(true);
                            }
                        }
                        selectedFieldsMap.replace(entityType, selectedFields);
                        idePluginPersistentState.saveState(IdePluginPersistentState.Key.SELECTED_FIELDS, new JSONObject(DefaultEntityFieldsUtil.entityFieldsToJson(selectedFieldsMap)));

                    }
                });
                if (selectedFields.contains(fieldMetadata)) {
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
        return defaultFields;
    }

    /**
     * Gets the selected fields from the popup
     *
     * @return the selected fields
     */
    public Set<String> getSelectedFields() {
        return selectedFields;
    }

    private void setSelectedFields(Set<String> fields) {
        selectedFields.removeAll(selectedFields);
        selectedFields.addAll(fields);
    }

    /**
     * Sets the fields in the current detailed view
     *
     * @param fields the fields from another detailed view tab with the same entity
     */
    public void setSelectedFieldsFromOtherTab(Set<String> fields) {
        setSelectedFields(fields);
        updateFieldsPanel(selectedFields, allFields);
        fieldsPanel.repaint();
        listeners.get(0).valueChanged(new SelectionEvent(this));
        if (defaultFields.containsAll(selectedFields) && selectedFields.containsAll(defaultFields)) {
            fieldsActionButton.setDefaultFieldsIcon(true);
            resetButton.setEnabled(false);
        } else {
            fieldsActionButton.setDefaultFieldsIcon(false);
            resetButton.setEnabled(true);
        }

    }
}
