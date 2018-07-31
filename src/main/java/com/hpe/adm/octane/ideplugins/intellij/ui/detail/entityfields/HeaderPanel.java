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

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.DetailsViewDefaultFields;
import com.hpe.adm.octane.ideplugins.intellij.ui.entityicon.EntityIconFactory;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import java.awt.*;

import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.MANUAL_TEST_RUN;
import static com.hpe.adm.octane.ideplugins.services.filtering.Entity.TEST_SUITE_RUN;

public class HeaderPanel extends JPanel {

    private JLabel entityIconLabel;
    private JTextField entityId;
    private JSeparator separatorIdName;
    private JSeparator separatorNamePhase;
    private JSeparator separatorPhaseButtons;
    private JTextField entityName;

    private AnAction saveSelectedPhaseAction;
    private AnAction refreshAction;
    private AnAction fieldsSelectAction;
    private AnAction commentAction;
    private ActionToolbar actionToolBar;
    private DefaultActionGroup buttonActionGroup;
    private JPanel panelControls;


    private EntityModelWrapper entityModelWrapper;
    private PhasePanel phasePanel;

    @Inject
    private EntityService entityService;

    @Inject
    public HeaderPanel(PhasePanel phasePanel) {
        this.phasePanel = phasePanel;

        UIManager.put("ComboBox.background", JBColor.background());
        UIManager.put("ComboBox.foreground", JBColor.foreground());
        UIManager.put("ComboBox.selectionBackground", JBColor.background());
        UIManager.put("ComboBox.selectionForeground", JBColor.foreground());

        setToolTipText("");
        setBorder(null);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0};
        setLayout(gridBagLayout);

        entityIconLabel = new JLabel();
        entityIconLabel.setHorizontalAlignment(SwingConstants.CENTER);
        GridBagConstraints gbc_entityIconLabel = new GridBagConstraints();
        gbc_entityIconLabel.insets = JBUI.insets(0, 10, 5, 5);
        gbc_entityIconLabel.gridx = 0;
        gbc_entityIconLabel.anchor = GridBagConstraints.WEST;
        add(entityIconLabel, gbc_entityIconLabel);

        entityId = new JTextField();
        entityId.setBorder(BorderFactory.createEmptyBorder());
        entityId.setHorizontalAlignment(SwingConstants.CENTER);
        entityId.setFont(new Font("Tahoma", Font.BOLD, 14));
        entityId.setEditable(false);
        GridBagConstraints gbc_entityId = new GridBagConstraints();
        gbc_entityId.insets = JBUI.insets(0, 0, 5, 5);
        gbc_entityId.gridx = 1;
        gbc_entityId.anchor = GridBagConstraints.WEST;
        add(entityId, gbc_entityId);

        separatorIdName = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator1 = new GridBagConstraints();
        gbc_separator1.gridx = 2;
        gbc_separator1.insets = JBUI.insets(5, 0, 5, 5);
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
        gbc_entityName.insets = JBUI.insetsRight(5);
        gbc_entityName.anchor = GridBagConstraints.WEST;
        add(entityName, gbc_entityName);

        separatorNamePhase = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator2 = new GridBagConstraints();
        gbc_separator2.gridx = 5;
        gbc_separator2.insets = JBUI.insets(5);
        gbc_separator2.fill = GridBagConstraints.VERTICAL;
        add(separatorNamePhase, gbc_separator2);

        GridBagConstraints gbc_phasePanel = new GridBagConstraints();
        gbc_phasePanel.gridx = 6;
        gbc_phasePanel.anchor = GridBagConstraints.WEST;
        add(phasePanel, gbc_phasePanel);

        separatorPhaseButtons = new JSeparator(SwingConstants.VERTICAL);
        GridBagConstraints gbc_separator3 = new GridBagConstraints();
        gbc_separator3.gridx = 7;
        gbc_separator3.insets = JBUI.insets(5, 5, 7, 5);
        gbc_separator3.fill = GridBagConstraints.VERTICAL;
        add(separatorPhaseButtons, gbc_separator3);

        buttonActionGroup = new DefaultActionGroup();
        panelControls = new JPanel(new BorderLayout());
        panelControls.setMinimumSize(new Dimension(175, 30));
        actionToolBar = ActionManager.getInstance().createActionToolbar("save | refresh | fields | open in browser | comments ", buttonActionGroup,
                true);
        GridBagConstraints gbc_actionButtons = new GridBagConstraints();
        gbc_actionButtons.insets = JBUI.insetsBottom(5);
        gbc_actionButtons.gridx = 8;
        gbc_actionButtons.anchor = GridBagConstraints.EAST;
        panelControls.add(actionToolBar.getComponent(), BorderLayout.CENTER);
        add(panelControls, gbc_actionButtons);
    }

    private void setEntityIcon(ImageIcon entityIcon) {
        entityIconLabel.setIcon(entityIcon);
    }

    private void setId(String id) {
        entityId.setText(id);
        entityId.setColumns(id.length());
        entityId.setMinimumSize(entityId.getPreferredSize());
    }

    private void setNameDetails(String nameDetails) {
        this.entityName.setText(nameDetails.trim());
        this.entityName.setCaretPosition(0);
        this.entityName.setMinimumSize(entityName.getPreferredSize());
    }

    public void setSaveButton(AnAction saveSelectedPhaseAction) {
        if (this.saveSelectedPhaseAction == null) {
            this.saveSelectedPhaseAction = saveSelectedPhaseAction;
            buttonActionGroup.addSeparator();
            buttonActionGroup.add(saveSelectedPhaseAction);
        }
    }

    public void setRefreshButton(AnAction refreshAction) {
        if (this.refreshAction == null) {
            this.refreshAction = refreshAction;
            buttonActionGroup.addSeparator();
            buttonActionGroup.add(refreshAction);
        }
    }

    public void setFieldSelectButton(AnAction fieldsSelectAction) {
        if (this.fieldsSelectAction == null) {
            this.fieldsSelectAction = fieldsSelectAction;
            buttonActionGroup.addSeparator();
            buttonActionGroup.add(fieldsSelectAction);
        }
    }

    public void setOpenInBrowserButton() {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(new EntityOpenInBrowser());
    }

    public void setCommentButton(AnAction commentAction) {
        if (this.commentAction == null) {
            this.commentAction = commentAction;
            buttonActionGroup.addSeparator();
            buttonActionGroup.add(commentAction);
        }
    }

    private void setPhaseDetails(FieldModel phaseDetails) {
        phasePanel.setPhaseDetails(phaseDetails);
    }

    public void setPhaseInHeader(boolean showPhase) {
        phasePanel.setPhaseInHeader(showPhase);
    }

    public Point getFieldsPopupLocation() {
        Component button = actionToolBar.getComponent().getComponent(actionToolBar.getComponent().getComponents().length - 2);
        return new Point(button.getLocationOnScreen().x + (int) button.getPreferredSize().getWidth(),
                button.getLocationOnScreen().y + (int) button.getPreferredSize().getHeight() + 8);
    }

    public void setEntityModel(EntityModelWrapper entityModelWrapper) {
        this.entityModelWrapper = entityModelWrapper;
        EntityIconFactory entityIconFactory = new EntityIconFactory(26, 26, 12);
        // icon
        setEntityIcon(new ImageIcon(entityIconFactory.getIconAsImage(entityModelWrapper.getEntityType())));
        // id
        setId(Util.getUiDataFromModel(entityModelWrapper.getValue(DetailsViewDefaultFields.FIELD_ID)));
        // name
        setNameDetails(Util.getUiDataFromModel(entityModelWrapper.getValue(DetailsViewDefaultFields.FIELD_NAME)));
        // phase
        setPhaseDetails(entityModelWrapper.getValue(DetailsViewDefaultFields.FIELD_PHASE));
        //setup target phase
        if (entityModelWrapper.getEntityType() != MANUAL_TEST_RUN && entityModelWrapper.getEntityType() != TEST_SUITE_RUN) {
            phasePanel.setEntityModelWrapper(entityModelWrapper);
            setPhaseInHeader(true);
        } else {
            setPhaseInHeader(false);
            //remove extra separator between phase and buttons
            remove(separatorPhaseButtons);
        }
    }

    private final class EntityOpenInBrowser extends AnAction {
        public EntityOpenInBrowser() {
            super("Open in browser the current entity", "Open in browser", IconLoader.findIcon(Constants.IMG_BROWSER_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            entityService.openInBrowser(entityModelWrapper.getEntityModel());
        }
    }
}
