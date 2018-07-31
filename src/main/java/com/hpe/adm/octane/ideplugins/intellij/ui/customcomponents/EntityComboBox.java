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

package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;


import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields;
import com.hpe.adm.octane.ideplugins.intellij.ui.listeners.SelectionEvent;
import com.hpe.adm.octane.ideplugins.intellij.ui.listeners.SelectionListener;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.EntityUtil;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.ui.VerticalFlowLayout;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;
import com.intellij.ui.RoundedLineBorder;
import com.intellij.ui.components.JBScrollPane;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class EntityComboBox extends JPanel {

    private boolean isMulti = false;
    private boolean filterable = true;

    private JLabel arrowButton;
    private JTextField editorLabel;
    private JSeparator separator;
    private JTextField searchField;
    private JFrame popupFrame;
    private JPanel popupRootPanel;
    private JScrollPane scrollPanel;
    private JPanel scrollRootPanel;

    private Collection<EntityModel> entities;
    private EntityLoader entityLoader;

    private List<SelectionListener> selectionListeners = new ArrayList<>();
    private List<EntityModel> selectedEntities = new ArrayList<>();

    private Timer delayTimer;

    public interface EntityLoader {
        public Collection<EntityModel> loadEntities(String searchQuery);
    }

    public EntityComboBox() {
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0, 0};
        gbl.columnWeights = new double[]{0.0, 0.0};
        setLayout(gbl);

        setBorder(new RoundedLineBorder(Color.GRAY, 5));

        editorLabel = new JTextField();
        editorLabel.setEditable(false);
        editorLabel.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
        GridBagConstraints gbc_editorLabel = new GridBagConstraints();
        gbc_editorLabel.anchor = GridBagConstraints.WEST;
        gbc_editorLabel.fill = GridBagConstraints.HORIZONTAL;
        gbc_editorLabel.gridx = 0;
        gbc_editorLabel.weightx = 1.0;
        add(editorLabel, gbc_editorLabel);

        separator = new JSeparator(SwingConstants.VERTICAL);
        separator.setBackground(Color.GRAY);
        GridBagConstraints gbc_separator = new GridBagConstraints();
        gbc_separator.gridx = 1;
        gbc_separator.fill = GridBagConstraints.VERTICAL;
        add(separator, gbc_separator);

        arrowButton = new JLabel(IconLoader.findIcon(Constants.IMG_ENTITY_COMBOBOX_ARROW));
        GridBagConstraints gbc_arrowButton = new GridBagConstraints();
        gbc_arrowButton.anchor = GridBagConstraints.WEST;
        gbc_arrowButton.gridx = 2;
        add(arrowButton, gbc_arrowButton);
        arrowButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (popupFrame == null) {
                    createPopup();
                    delayTimer.restart();
                }
                popupFrame.setLocation(arrowButton.getLocationOnScreen().x + (int) arrowButton.getBounds().getWidth() - (int) popupFrame.getPreferredSize().getWidth(),
                        arrowButton.getLocationOnScreen().y + (int) arrowButton.getBounds().getHeight());
                popupFrame.setVisible(!popupFrame.isVisible());

            }
        });

        delayTimer = new Timer(500, e -> {
            showLoading();
            new Thread(() -> {
                if(filterable){
                    entities = entityLoader.loadEntities(searchField.getText());
                } else {
                    entities = entityLoader.loadEntities("");
                }
                scrollRootPanel.removeAll();
                if (entities == null || entities.isEmpty()) {
                    createNoResultsPanel();
                } else if (!isMulti) {
                    createSingleSelectionUI();
                } else if (isMulti) {
                    createMultiSelectionUI();
                }
            }).start();
            delayTimer.stop();
        });
    }

    private void createSingleSelectionUI() {
        scrollRootPanel.removeAll();
        entities.forEach(entityModel -> {
            JMenuItem menuItem = new JMenuItem();
            menuItem.setText(getLabelText(entityModel));
            menuItem.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    selectedEntities.clear();
                    selectedEntities.add(entityModel);
                    selectionListeners.forEach(l -> l.valueChanged(new SelectionEvent(e)));
                    editorLabel.setText(menuItem.getText());
                    popupFrame.setVisible(false);
                }

                @Override
                public void mouseEntered(MouseEvent e) {
                    menuItem.setBackground((Color) UIManager.get("CheckBoxMenuItem.selectionBackground"));
                    menuItem.setForeground((Color) UIManager.get("CheckBoxMenuItem.selectionForeground"));
                    menuItem.revalidate();
                }

                @Override
                public void mouseExited(MouseEvent e) {
                    menuItem.setBackground((Color) UIManager.get("CheckBoxMenuItem.Background"));
                    menuItem.setForeground((Color) UIManager.get("CheckBoxMenuItem.Foreground"));
                    menuItem.revalidate();
                }
            });
            scrollRootPanel.add(menuItem);
        });
        scrollRootPanel.revalidate();
        scrollRootPanel.repaint();
    }

    private void createMultiSelectionUI() {
        scrollRootPanel.removeAll();
        entities.forEach(entityModel -> {
            FieldMenuItem menuItem = new FieldMenuItem(getLabelText(entityModel));
            menuItem.setState(EntityUtil.containsEntityModel(selectedEntities, entityModel));
            menuItem.setPreferredSize(new Dimension((int) menuItem.getBounds().getHeight(), (int) scrollRootPanel.getPreferredSize().getWidth()));
            menuItem.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    // it's being selected
                    if (menuItem.isSelected() && !EntityUtil.containsEntityModel(selectedEntities, entityModel)) {
                        selectedEntities.add(entityModel);
                    }
                    // it's being de-selected
                    else if (!menuItem.isSelected() && EntityUtil.containsEntityModel(selectedEntities, entityModel)) {
                        EntityUtil.removeEntityModel(selectedEntities, entityModel);
                    }
                    selectionListeners.forEach(l -> l.valueChanged(new SelectionEvent(e)));
                    setEditorText();
                }
            });
            menuItem.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseEntered(MouseEvent e) {
                    menuItem.setBackground((Color) UIManager.get("CheckBoxMenuItem.selectionBackground"));
                    menuItem.setForeground((Color) UIManager.get("CheckBoxMenuItem.selectionForeground"));
                    menuItem.revalidate();
                }

                @Override
                public void mouseExited(MouseEvent e) {
                    menuItem.setBackground((Color) UIManager.get("CheckBoxMenuItem.Background"));
                    menuItem.setForeground((Color) UIManager.get("CheckBoxMenuItem.Foreground"));
                    menuItem.revalidate();
                }
            });


            scrollRootPanel.add(menuItem);
        });
        scrollRootPanel.revalidate();
        scrollRootPanel.repaint();
    }

    private void createNoResultsPanel() {
        scrollRootPanel.removeAll();
        JMenuItem noResults = new JMenuItem("No results");
        noResults.setForeground(Color.RED);
        scrollRootPanel.add(noResults);
        scrollRootPanel.revalidate();
        scrollRootPanel.repaint();
    }


    public void setFilterable(boolean filterable) {
        this.filterable = filterable;
    }

    public void setEntityLoader(EntityLoader entityLoader) {
        this.entityLoader = entityLoader;
    }

    public void createPopup() {
        popupFrame = new JFrame();

        popupRootPanel = new JPanel();
        popupRootPanel.setBorder(new MatteBorder(2, 2, 2, 2, JBColor.border()));
        GridBagLayout gbl = new GridBagLayout();
        gbl.columnWidths = new int[]{0, 0};
        gbl.rowHeights = new int[]{0};
        gbl.columnWeights = new double[]{0.5, 0.3, 0.0};
        gbl.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0};
        popupRootPanel.setLayout(gbl);


        if (filterable) {
            searchField = new JXTextField("Search fields");
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
            gbcSearchField.insets = new Insets(5, 5, 0, 5);
            gbcSearchField.anchor = GridBagConstraints.NORTH;
            gbcSearchField.fill = GridBagConstraints.HORIZONTAL;
            gbcSearchField.gridx = 0;
            gbcSearchField.gridy = 0;
            popupRootPanel.add(searchField, gbcSearchField);
        }

        scrollRootPanel = new JPanel();
        scrollRootPanel.setLayout(new VerticalFlowLayout(VerticalFlowLayout.TOP, 0, 5, true, false));
        scrollPanel = new JBScrollPane(scrollRootPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPanel.setPreferredSize(new Dimension(200, 200));
        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.fill = GridBagConstraints.BOTH;
        gbc1.insets = new Insets(5, 5, 5, 5);
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        popupRootPanel.add(scrollPanel, gbc1);

        popupFrame.setContentPane(popupRootPanel);
        popupFrame.addWindowFocusListener(new WindowFocusListener() {
            @Override
            public void windowGainedFocus(WindowEvent e) {
            }

            @Override
            public void windowLostFocus(WindowEvent e) {
                popupFrame.setVisible(false);
            }
        });

        popupFrame.setAlwaysOnTop(true);
        popupFrame.setUndecorated(true);
        popupFrame.pack();
    }

    private void searchFieldAction() {
        delayTimer.restart();
    }

    private void showLoading() {
        scrollRootPanel.removeAll();
        LoadingWidget loadingWidget = new LoadingWidget();
        scrollRootPanel.add(loadingWidget);
        scrollRootPanel.repaint();
        popupFrame.revalidate();
        popupFrame.repaint();
    }

    public void clearEditor() {
        editorLabel.setText("");
        selectedEntities.clear();
    }

    public void setMultiSelect(boolean multiSelect) {
        this.isMulti = multiSelect;
    }

    private String getLabelText(EntityModel entityModel) {
        String nameField;
        if (Entity.getEntityType(entityModel) == Entity.WORKSPACE_USER) {
            nameField = DetailsViewDefaultFields.FIELD_FULL_NAME;
        } else {
            nameField = DetailsViewDefaultFields.FIELD_NAME;
        }

        return Util.getUiDataFromModel(entityModel.getValue(nameField));
    }

    public void setSelectedEntities(Collection<EntityModel> entities) {
        this.selectedEntities.clear();
        selectedEntities.addAll(entities);
        setEditorText();
    }

    public void setSelectedEntity(EntityModel entityModel) {
        selectedEntities.clear();
        selectedEntities.add(entityModel);
        setEditorText();
    }

    private void setEditorText() {
        editorLabel.setText(selectedEntities
                .stream()
                .map(entityModel -> getLabelText(entityModel))
                .collect(Collectors.joining(" | ")));
    }

    public boolean isMultiSelect() {
        return isMulti;
    }

    public void addSelectionListener(SelectionListener listener) {
        selectionListeners.add(listener);
    }

    public EntityModel getSelectedEntity() {
        return selectedEntities.get(0);
    }

    public Collection<EntityModel> getSelectedEntities() {
        return selectedEntities;
    }
}
