package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.util.IconLoader;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class PhaseDropDownMenu extends JPanel {
    public static final String MOVE_TO = "Move to: ";
    public static final String MOVED_TO = "Moved to: ";
    private static final String TOOLTIP_BLOCKED_PHASE = "You must save first before doing any more changes to phase";
    private static final String TOOLTIP_CLICKABLE_PHASE = "Click here to choose your desired next phase";

    private JLabel targetPhaseLabel;
    private JLabel arrow;
    private JPopupMenu popupMenu;

    private Map<JMenuItem, FieldModel> labelPhaseMap;

    private FieldModel selectedPhase;

    public PhaseDropDownMenu() {
        GridBagLayout gbl_phasePanel = new GridBagLayout();
        gbl_phasePanel.columnWidths = new int[]{0, 0};
        gbl_phasePanel.columnWeights = new double[]{0.0, 0.0};
        setLayout(gbl_phasePanel);
        labelPhaseMap = new HashMap<>();
        targetPhaseLabel = new JLabel(MOVE_TO + "Loading phase ...");
        targetPhaseLabel.setForeground(Color.BLUE);
        targetPhaseLabel.setFont(new Font("Arial", Font.BOLD, 14));
        GridBagConstraints gbc_targetPhaseLabel = new GridBagConstraints();
        gbc_targetPhaseLabel.anchor = GridBagConstraints.WEST;
        gbc_targetPhaseLabel.weightx = 0.9;
        gbc_targetPhaseLabel.gridx = 0;
        gbc_targetPhaseLabel.insets = new Insets(0, 5, 0, 5);
        add(targetPhaseLabel, gbc_targetPhaseLabel);
    }

    public void setPhaseDetails(FieldModel currentPhase) {
        this.selectedPhase = currentPhase;
    }

    public void addItems(List<EntityModel> phasesList) {

        //need to add the first item from the list to the target phase and create the popup from the rest
        targetPhaseLabel.setText(MOVE_TO + Util.getUiDataFromModel(phasesList.get(0).getValue("target_phase"), "name"));
        targetPhaseLabel.setForeground(Color.BLUE);
        targetPhaseLabel.setFont(new Font("Arial", Font.BOLD, 14));
        targetPhaseLabel.setToolTipText(TOOLTIP_CLICKABLE_PHASE);
        targetPhaseLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                selectedPhase = labelPhaseMap.values().stream().filter(en ->
                    (Util.getUiDataFromModel((FieldModel) en, "name").equals(targetPhaseLabel.getText()))).collect(Collectors.toList()).get(0);
                targetPhaseLabel.setText(MOVED_TO + Util.getUiDataFromModel(selectedPhase, "name"));
                targetPhaseLabel.setToolTipText(TOOLTIP_BLOCKED_PHASE);
                targetPhaseLabel.setEnabled(false);
            }
        });
        phasesList.stream().forEach(e -> {
            String phase = Util.getUiDataFromModel(e.getValue("target_phase"), "name");
            //don't put the phase which is already in the target label into the popup menu
            if(!targetPhaseLabel.getText().equals(phase)){
                JMenuItem menuItem = new JMenuItem(phase);
                labelPhaseMap.put(menuItem, e.getValue("target_phase"));
            }
        });

        if (phasesList.size() > 1) {
            //create the arrow
            arrow = new JLabel();
            arrow.setIcon(IconLoader.findIcon(Constants.IMG_PHASE_DROPDOWN));
            arrow.setToolTipText(TOOLTIP_CLICKABLE_PHASE);
            arrow.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    super.mouseClicked(e);
                    popupMenu.setVisible(true);
                    popupMenu.setLocation(arrow.getLocationOnScreen().x + (int) arrow.getPreferredSize().getWidth() - (int) popupMenu.getPreferredSize().getWidth(),
                            arrow.getLocationOnScreen().y + (int) arrow.getPreferredSize().getHeight());
                }
            });
            GridBagConstraints gbc_arrow = new GridBagConstraints();
            gbc_arrow.anchor = GridBagConstraints.WEST;
            gbc_arrow.gridx = 1;
            gbc_arrow.weightx = 0.1;
            gbc_arrow.insets = new Insets(0, 5, 0, 5);
            add(arrow, gbc_arrow);

            //create the popup frame
            popupMenu = new JPopupMenu();

            JPanel rootPanel = new JPanel();
            rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
            List<JMenuItem> labels = new ArrayList<>(labelPhaseMap.keySet());
            for (int i = 1; i < phasesList.size(); i++) {
                JMenuItem lbl = labels.get(i);
                popupMenu.add(lbl);
                lbl.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent e) {
                        super.mouseClicked(e);
                        selectedPhase = labelPhaseMap.get(lbl);
                        targetPhaseLabel.setText(MOVED_TO + Util.getUiDataFromModel(selectedPhase, "name"));
                        targetPhaseLabel.setToolTipText(TOOLTIP_BLOCKED_PHASE);
                        targetPhaseLabel.setEnabled(false);
                        remove(arrow);
                        popupMenu.setVisible(false);
                    }
                });
            }
        }
    }

    public FieldModel getSelectedItem() {
        return selectedPhase;
    }
}
