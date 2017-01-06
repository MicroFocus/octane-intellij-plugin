package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PhaseComboBox;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions.SaveAction;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.ActionToolbar;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import org.jdesktop.swingx.JXHyperlink;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.util.Collection;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER;

public class HeaderPanel extends JPanel {
    private JXLabel nameDetails;
    private JXLabel phaseDetails;
    private JBScrollPane refreshButtonPanel;
    private DefaultActionGroup buttonActionGroup;
    private JXHyperlink entityLinkToBrowser;
    private PhaseComboBox comboBox;


    public HeaderPanel() {
        UIManager.put("ComboBox.background", JBColor.background());
        UIManager.put("ComboBox.foreground", JBColor.foreground());
        UIManager.put("ComboBox.selectionBackground", JBColor.background());
        UIManager.put("ComboBox.selectionForeground", JBColor.foreground());

        setToolTipText("");
        setBorder(null);
        setLayout(new BorderLayout(0, 0));

        JXPanel nameAndIconPanel = new JXPanel();
        nameAndIconPanel.setBorder(null);
        FlowLayout flowLayout = (FlowLayout) nameAndIconPanel.getLayout();
        flowLayout.setHgap(0);
        add(nameAndIconPanel, BorderLayout.WEST);

        nameDetails = new JXLabel();
        nameDetails.setIcon(new ImageIcon(HeaderPanel.class.getResource("/images/defectIcon.png")));
        nameAndIconPanel.add(nameDetails);

        entityLinkToBrowser = new JXHyperlink();
        entityLinkToBrowser.setClickedColor(new Color(102, 205, 170));
        entityLinkToBrowser.setBorder(new EmptyBorder(0, 5, 0, 0));
        entityLinkToBrowser.setUnclickedColor(JBColor.foreground());
        entityLinkToBrowser.setText("Name");
        entityLinkToBrowser.setBorderPainted(false);
        nameAndIconPanel.add(entityLinkToBrowser);

        JXPanel phasePanel = new JXPanel();
        phasePanel.setBorder(null);
        FlowLayout flowLayout_1 = (FlowLayout) phasePanel.getLayout();
        flowLayout_1.setHgap(0);
        add(phasePanel, BorderLayout.EAST);

        JXLabel currentPhaseLabel = new JXLabel();
        currentPhaseLabel.setBorder(new EmptyBorder(0, 0, 0, 5));
        currentPhaseLabel.setFont(new Font("Tahoma", Font.BOLD, 11));
        currentPhaseLabel.setText("Current phase:");
        phasePanel.add(currentPhaseLabel);

        phaseDetails = new JXLabel();
        phaseDetails.setBorder(new EmptyBorder(0, 0, 0, 10));
        phaseDetails.setFont(new Font("Tahoma", Font.ITALIC, 11));
        phaseDetails.setText("phase");
        phasePanel.add(phaseDetails);


        comboBox = new PhaseComboBox();
        comboBox.setEditable(true);
        comboBox.setPreferredSize(new Dimension(110, 20));
        phasePanel.add(comboBox);

        refreshButtonPanel = new JBScrollPane();
        refreshButtonPanel.setBorder(null);
        refreshButtonPanel.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_NEVER);
        refreshButtonPanel.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        refreshButtonPanel.setMinimumSize(new Dimension(0, 0));
        phasePanel.add(refreshButtonPanel);


    }

    public void setNameDetails(String nameDetails) {
        this.entityLinkToBrowser.setText(nameDetails);
    }

    public void setPhaseDetails(String phaseDetails) {
        this.phaseDetails.setText(phaseDetails);
    }

    public void setEntityIcon(ImageIcon entityIcon) {
        this.nameDetails.setIcon(entityIcon);
    }

    public void createRefreshButton(AnAction refreshAction) {
        buttonActionGroup = new DefaultActionGroup();
        buttonActionGroup.addSeparator();
//        buttonActionGroup.add(new PhaseComboBoxAction());
        buttonActionGroup.add(new SaveAction());
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(refreshAction);

        final ActionToolbar actionToolBar = ActionManager.getInstance().createActionToolbar("refresh | save", buttonActionGroup, true);
        final JXPanel buttonsPanel = new JXPanel(new BorderLayout());
        buttonsPanel.add(actionToolBar.getComponent(), BorderLayout.CENTER);

        refreshButtonPanel.setViewportView(buttonsPanel);
    }
    public void addActionToEntityLink(AbstractAction action) {
        entityLinkToBrowser.setAction(action);

    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        comboBox.addItems(phasesList);
    }






}
