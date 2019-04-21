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

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.ui.JBPopupMenu;
import com.intellij.openapi.util.IconLoader;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class PhaseDropDownMenu extends JPanel {

    private static final Color enabledColor = new Color(30, 144, 255);
    private static final String MOVE_TO = "Move to: ";
    private static final String MOVED_TO = "Moved to: ";
    private static final String LOADING_PHASE = "Loading phase ...";
    private static final String TOOLTIP_BLOCKED_PHASE = "You must save first before doing any more changes to phase";
    private static final String TOOLTIP_CLICKABLE_PHASE = "Click here to choose your desired next phase";

    private JLabel targetPhaseLabel;
    private JLabel arrow;
    private JBPopupMenu popupMenu;

    private Map<JMenuItem, FieldModel> labelPhaseMap;
    private List<EntityModel> phaseList;
    private FieldModel selectedPhase;

    private EntityModelWrapper entityModelWrapper;

    @Inject
    private EntityService entityService;

    public PhaseDropDownMenu() {
        GridBagLayout gbl_phasePanel = new GridBagLayout();
        gbl_phasePanel.columnWidths = new int[]{0, 0};
        gbl_phasePanel.columnWeights = new double[]{0.0, 0.0};
        setLayout(gbl_phasePanel);
        labelPhaseMap = new HashMap<>();

        targetPhaseLabel = new JLabel(LOADING_PHASE);
        targetPhaseLabel.setEnabled(false);

        targetPhaseLabel.setFont(new Font(targetPhaseLabel.getFont().getName(), Font.BOLD, 14));
        targetPhaseLabel.setCursor(new Cursor(Cursor.HAND_CURSOR));

        GridBagConstraints gbc_targetPhaseLabel = new GridBagConstraints();
        gbc_targetPhaseLabel.anchor = GridBagConstraints.WEST;
        gbc_targetPhaseLabel.weightx = 0.9;
        gbc_targetPhaseLabel.gridx = 0;
        gbc_targetPhaseLabel.insets = new Insets(5, 5, 5, 5);
        add(targetPhaseLabel, gbc_targetPhaseLabel);
    }

    /**
     * This method will set the current phase as the one selected, in the case that user clicks save we don't want to
     * change the phase also.
     * Moreover after a save the targetPhaseLabel is activated again
     *
     * @param currentPhase - the current phase of the entity
     */
    public void setPhaseDetails(FieldModel currentPhase) {
        // re enable the target phase if the user changed the phase
        targetPhaseLabel.setEnabled(true);
        targetPhaseLabel.setForeground(enabledColor);
        this.selectedPhase = currentPhase;
    }

    public void addItems(List<EntityModel> phasesList) {
        this.phaseList = phasesList;
        // need to add the first item from the list to the target phase and
        // create the popup from the rest
        if ("No transition".equals(Util.getUiDataFromModel(phasesList.get(0).getValue("target_phase")))) {
            targetPhaseLabel.setText("No transition");
            targetPhaseLabel.setEnabled(false);
        } else {
            targetPhaseLabel.setText(MOVE_TO + Util.getUiDataFromModel(phasesList.get(0).getValue("target_phase"), "name"));
            targetPhaseLabel.setToolTipText(TOOLTIP_CLICKABLE_PHASE);
            targetPhaseLabel.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    selectedPhase = phaseList.stream()
                            .filter(en -> (targetPhaseLabel.getText().contains(Util.getUiDataFromModel(en.getValue("target_phase"), "name"))))
                            .collect(Collectors.toList()).get(0).getValue("target_phase");
                    targetPhaseLabel.setText(MOVED_TO + Util.getUiDataFromModel(selectedPhase, "name"));
                    targetPhaseLabel.setToolTipText(TOOLTIP_BLOCKED_PHASE);
                    targetPhaseLabel.setEnabled(false);
                    if (arrow != null) {
                        remove(arrow);
                    }
                    entityModelWrapper.setValue(new ReferenceFieldModel("phase", ((ReferenceFieldModel) selectedPhase).getValue()));
                }
            });
        }
        labelPhaseMap.clear();
        phasesList.stream().forEach(e -> {
            String phase = Util.getUiDataFromModel(e.getValue("target_phase"), "name");
            // don't put the phase which is already in the target label into the
            // popup menu
            if (!targetPhaseLabel.getText().contains(phase)) {
                JMenuItem menuItem = new JMenuItem(phase);
                labelPhaseMap.put(menuItem, e.getValue("target_phase"));
            }
        });

        if (phasesList.size() > 1) {
            // only create new arrow if there isn't one already
            if (arrow == null) {
                // create the arrow
                arrow = new JLabel();

                Icon dropdown = getDropdownIcon();
                arrow.setIcon(dropdown);
                arrow.setToolTipText(TOOLTIP_CLICKABLE_PHASE);
                arrow.setCursor(new Cursor(Cursor.HAND_CURSOR));
                arrow.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent e) {
                        //first show computes the width of the popupMenu
                        //second one shows it correctly - just dev stuff
                        popupMenu.show(arrow, 0, arrow.getBounds().height);
                        popupMenu.show(arrow, -popupMenu.getWidth() + arrow.getBounds().width / 2, arrow.getBounds().height);
                        popupMenu.applyComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
                    }
                });
            }

            GridBagConstraints gbc_arrow = new GridBagConstraints();
            gbc_arrow.anchor = GridBagConstraints.WEST;
            gbc_arrow.gridx = 1;
            gbc_arrow.weightx = 0.1;
            gbc_arrow.insets = new Insets(5, 5, 5, 5);
            add(arrow, gbc_arrow);

            // create the popup
            popupMenu = new JBPopupMenu();

            JPanel rootPanel = new JPanel();
            rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
            List<JMenuItem> labels = new ArrayList<>(labelPhaseMap.keySet());
            for (int i = 0; i < labelPhaseMap.keySet().size(); i++) {
                JMenuItem lbl = labels.get(i);
                popupMenu.add(lbl);
                lbl.addActionListener((a) -> {
                    selectedPhase = labelPhaseMap.get(lbl);
                    targetPhaseLabel.setText(MOVED_TO + Util.getUiDataFromModel(selectedPhase, "name"));
                    targetPhaseLabel.setToolTipText(TOOLTIP_BLOCKED_PHASE);
                    targetPhaseLabel.setEnabled(false);
                    remove(arrow);
                    popupMenu.setVisible(false);
                    entityModelWrapper.setValue(new ReferenceFieldModel("phase", ((ReferenceFieldModel) selectedPhase).getValue()));
                });
            }
        }
    }

    private Icon getDropdownIcon() {
        Icon dropdown;
        IconLoader.findIcon(Constants.IMG_PHASE_DROPDOWN);
        if(UIUtil.isUnderDarcula()) {
            dropdown = IconLoader.findIcon(Constants.IMG_PHASE_DROPDOWN_DARKULA);
        } else {
            dropdown = IconLoader.findIcon(Constants.IMG_PHASE_DROPDOWN);
        }
        return dropdown;
    }

    public void setEntityModelWrapper(EntityModelWrapper entityModelWrapper) {
        this.entityModelWrapper = entityModelWrapper;
        setupPhaseDetails();
    }

    public void setupPhaseDetails() {
        RestUtil.runInBackground(() -> {
            String currentPhaseId = Util.getUiDataFromModel(entityModelWrapper.getValue("phase"), "id");
            return entityService.findPossibleTransitionFromCurrentPhase(entityModelWrapper.getEntityType(), currentPhaseId);
        }, (possibleTransitions) -> {
            if (possibleTransitions.isEmpty()) {
                possibleTransitions.add(new EntityModel("target_phase", "No transition"));
                addItems((List<EntityModel>) possibleTransitions);
            } else {
                addItems((List<EntityModel>) possibleTransitions);
            }
        }, null, "Failed to get possible transitions", "fetching possible transitions");
    }
}
