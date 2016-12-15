package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.ActionToolbar;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.ui.components.JBScrollPane;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER;

public class HeaderPanel extends JPanel {
    private JXLabel nameDetails;
    private JXLabel phaseDetails;
    private JBScrollPane refreshButtonPanel;
    private DefaultActionGroup buttonActionGroup;


    public HeaderPanel() {
        setLayout(new BorderLayout(0, 0));

        JXPanel nameAndIconPanel = new JXPanel();
        FlowLayout flowLayout = (FlowLayout) nameAndIconPanel.getLayout();
        flowLayout.setHgap(0);
        add(nameAndIconPanel, BorderLayout.WEST);

        nameDetails = new JXLabel();
        nameDetails.setIcon(new ImageIcon(HeaderPanel.class.getResource("/images/defectIcon.png")));
        nameDetails.setText("Name");
        nameAndIconPanel.add(nameDetails);

        JXPanel phasePanel = new JXPanel();
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

        JXLabel nextPhaseLink = new JXLabel();
        nextPhaseLink.setFont(new Font("Tahoma", Font.BOLD, 11));
        nextPhaseLink.setText("|  Move to next phase");
        nextPhaseLink.setVisible(false);
        phasePanel.add(nextPhaseLink);

        refreshButtonPanel = new JBScrollPane();
        refreshButtonPanel.setBorder(null);
        refreshButtonPanel.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_NEVER);
        refreshButtonPanel.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        refreshButtonPanel.setMinimumSize(new Dimension(0, 0));
        phasePanel.add(refreshButtonPanel);


    }

    public void setNameDetails(String nameDetails) {
        this.nameDetails.setText(nameDetails);
    }

    public void setPhaseDetails(String phaseDetails) {
        this.phaseDetails.setText(phaseDetails);
    }

    public void setEntityIcon(ImageIcon entityIcon) {
        this.nameDetails.setIcon(entityIcon);
    }


    public void setRefreshButton() {
        this.refreshButtonPanel.setVisible(true);
    }

    public void createRefreshButton(AnAction anAction) {
        buttonActionGroup = new DefaultActionGroup();
        buttonActionGroup.add(anAction);

        final ActionToolbar actionToolBar = ActionManager.getInstance().createActionToolbar("refresh", buttonActionGroup, true);
        final JXPanel buttonsPanel = new JXPanel(new BorderLayout());
        buttonsPanel.add(actionToolBar.getComponent(), BorderLayout.CENTER);

        refreshButtonPanel.setViewportView(buttonsPanel);
    }


}
