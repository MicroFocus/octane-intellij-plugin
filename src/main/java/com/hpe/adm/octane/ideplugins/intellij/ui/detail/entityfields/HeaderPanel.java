/*
 *  © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  http://www.apache.org/licenses/LICENSE-2.0
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.ActionToolbar;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.ui.JBColor;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import java.awt.*;
import java.util.Collection;

public class HeaderPanel extends JPanel {

    private JLabel entityIconLabel;
    private JTextField entityId;
    private JSeparator separatorIdName;
    private JSeparator separatorNamePhase;
    private JSeparator separatorPhaseButtons;
    private JTextField entityName;

    private AnAction saveSelectedPhaseAction;
    private ActionToolbar actionToolBar;
    private DefaultActionGroup buttonActionGroup;
    private JPanel panelControls;

    private PhasePanel phasePanel;

    public HeaderPanel() {
        UIManager.put("ComboBox.background", JBColor.background());
        UIManager.put("ComboBox.foreground", JBColor.foreground());
        UIManager.put("ComboBox.selectionBackground", JBColor.background());
        UIManager.put("ComboBox.selectionForeground", JBColor.foreground());

        setToolTipText("");
        setBorder(null);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[] { 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0 };
        setLayout(gridBagLayout);

        entityIconLabel = new JLabel();
        entityIconLabel.setHorizontalAlignment(SwingConstants.CENTER);
        GridBagConstraints gbc_entityIconLabel = new GridBagConstraints();
        gbc_entityIconLabel.insets = new Insets(0, 10, 5, 5);
        gbc_entityIconLabel.gridx = 0;
        gbc_entityIconLabel.anchor = GridBagConstraints.WEST;
        add(entityIconLabel, gbc_entityIconLabel);

        entityId = new JTextField();
        entityId.setBorder(BorderFactory.createEmptyBorder());
        entityId.setHorizontalAlignment(SwingConstants.CENTER);
        entityId.setFont(new Font("Tahoma", Font.BOLD, 14));
        entityId.setEditable(false);
        GridBagConstraints gbc_entityId = new GridBagConstraints();
        gbc_entityId.insets = new Insets(0, 0, 5, 5);
        gbc_entityId.gridx = 1;
        gbc_entityId.anchor = GridBagConstraints.WEST;
        add(entityId, gbc_entityId);

        separatorIdName = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator1 = new GridBagConstraints();
        gbc_separator1.gridx = 2;
        gbc_separator1.insets = new Insets(5, 0, 5, 5);
        gbc_separator1.fill = GridBagConstraints.VERTICAL;
        add(separatorIdName, gbc_separator1);

        entityName = new JTextField();
        entityName.setColumns(110);
        entityName.setBorder(BorderFactory.createEmptyBorder());
        entityName.setBackground(UIUtil.getLabelBackground());
        entityName.setFont(new Font("Tahoma", Font.PLAIN, 14));
        GridBagConstraints gbc_entityName = new GridBagConstraints();
        gbc_entityName.gridx = 3;
        gbc_entityName.gridwidth = 2;
        gbc_entityName.insets = new Insets(5, 5, 5, 5);
        gbc_entityName.anchor = GridBagConstraints.WEST;
        add(entityName, gbc_entityName);

        separatorNamePhase = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator2 = new GridBagConstraints();
        gbc_separator2.gridx = 5;
        gbc_separator2.insets = new Insets(5, 5, 5, 5);
        gbc_separator2.fill = GridBagConstraints.VERTICAL;
        add(separatorNamePhase, gbc_separator2);

        phasePanel = new PhasePanel();
        phasePanel.setVisible(true);
        GridBagConstraints gbc_phasePanel = new GridBagConstraints();
        gbc_phasePanel.insets = new Insets(0, 0, 0, 0);
        gbc_phasePanel.gridx = 6;
        gbc_phasePanel.anchor = GridBagConstraints.WEST;
        add(phasePanel, gbc_phasePanel);

        separatorPhaseButtons = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator3 = new GridBagConstraints();
        gbc_separator3.gridx = 7;
        gbc_separator3.insets = new Insets(5, 5, 7, 5);
        gbc_separator3.fill = GridBagConstraints.VERTICAL;
        add(separatorPhaseButtons, gbc_separator3);

        buttonActionGroup = new DefaultActionGroup();
        panelControls = new JPanel(new BorderLayout());
        panelControls.setMinimumSize(new Dimension(175, 25));
        actionToolBar = ActionManager.getInstance().createActionToolbar("save | refresh | fields | open in browser | comments ", buttonActionGroup,
                true);
        GridBagConstraints gbc_actionButtons = new GridBagConstraints();
        gbc_actionButtons.insets = new Insets(0, 0, 5, 0);
        gbc_actionButtons.gridx = 8;
        gbc_actionButtons.anchor = GridBagConstraints.EAST;
        panelControls.add(actionToolBar.getComponent(), BorderLayout.CENTER);
        add(panelControls, gbc_actionButtons);
    }

    public void setEntityIcon(ImageIcon entityIcon) {
        entityIconLabel.setIcon(entityIcon);
    }

    public void setId(String id) {
        entityId.setText(id);
        entityId.setColumns(id.length());
        entityId.setMinimumSize(entityId.getPreferredSize());
    }

    public void setNameDetails(String nameDetails) {
        this.entityName.setText(nameDetails.trim());
        this.entityName.setCaretPosition(0);
        this.entityName.setMinimumSize(entityName.getPreferredSize());
    }

    public void setSaveButton(AnAction saveSelectedPhaseAction) {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(saveSelectedPhaseAction);
        this.saveSelectedPhaseAction = saveSelectedPhaseAction;
    }

    public void removeSaveSelectedPhaseButton() {
        buttonActionGroup.remove(this.saveSelectedPhaseAction);
    }

    public void setRefreshButton(AnAction refreshAction) {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(refreshAction);
    }

    public void setFieldSelectButton(AnAction fieldsSelectAction) {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(fieldsSelectAction);
    }

    public void setOpenInBrowserButton(AnAction openInBrowserAction) {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(openInBrowserAction);
    }

    public void setCommentButton(AnAction commentAction) {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(commentAction);
    }

    public void setPhaseDetails(String phaseDetails) {
        phasePanel.setPhaseDetails(phaseDetails);
    }

    public void setPhaseInHeader(boolean showPhase) {
        phasePanel.setPhaseInHeader(showPhase);
    }

    public EntityModel getSelectedTransition() {
        return phasePanel.getSelectedTransition();
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        phasePanel.setPossiblePhasesForEntity(phasesList);
    }

    public Point getFieldsPopupLocation() {
        Component button = actionToolBar.getComponent().getComponent(actionToolBar.getComponent().getComponents().length - 1);
        return new Point(button.getLocationOnScreen().x + (int) button.getPreferredSize().getWidth(),
                button.getLocationOnScreen().y + (int) button.getPreferredSize().getHeight() + 8);
    }

}
