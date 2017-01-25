package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PhaseComboBox;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.ActionToolbar;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXHyperlink;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;

public class HeaderPanel extends JPanel {

    private JLabel entityIconLabel;
    private JXHyperlink entityLinkToBrowser;
    
    private JXPanel phasePanel;
    
    private JXLabel phaseDetails;
    private JXLabel currentPhaseLabel;
    private JXLabel moveToLabel;
    private PhaseComboBox phaseComboBox;    
    private AnAction saveSelectedPhaseAction;

    private JPanel panelControls;
    private DefaultActionGroup buttonActionGroup;



    public HeaderPanel() {
        UIManager.put("ComboBox.background", JBColor.background());
        UIManager.put("ComboBox.foreground", JBColor.foreground());
        UIManager.put("ComboBox.selectionBackground", JBColor.background());
        UIManager.put("ComboBox.selectionForeground", JBColor.foreground());

        setToolTipText("");
        setBorder(null);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 30, 0, 0};
        gridBagLayout.rowHeights = new int[]{40, 0};
        gridBagLayout.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, Double.MIN_VALUE};
        setLayout(gridBagLayout);

        entityLinkToBrowser = new JXHyperlink();
        entityLinkToBrowser.setFont(new Font("Tahoma", Font.PLAIN, 13));
        entityLinkToBrowser.setBorder(new EmptyBorder(0, 5, 0, 0));
        entityLinkToBrowser.setUnclickedColor(JBColor.foreground());
        entityLinkToBrowser.setClickedColor(JBColor.foreground());
        entityLinkToBrowser.setBorderPainted(false);
        entityLinkToBrowser.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                entityLinkToBrowser.setForeground(Color.blue);
            }
            @Override
            public void mouseExited(MouseEvent e) {
                entityLinkToBrowser.setForeground(JBColor.foreground());
            }
        });
        
        entityIconLabel = new JLabel("");
        entityIconLabel.setHorizontalAlignment(SwingConstants.CENTER);
        GridBagConstraints gbc_entityIconLabel = new GridBagConstraints();
        gbc_entityIconLabel.insets = new Insets(0, 20, 0, 5);
        gbc_entityIconLabel.gridx = 0;
        gbc_entityIconLabel.gridy = 0;
        add(entityIconLabel, gbc_entityIconLabel);
        GridBagConstraints gbc_labelReplaceme = new GridBagConstraints();
        gbc_labelReplaceme.fill = GridBagConstraints.HORIZONTAL;
        gbc_labelReplaceme.insets = new Insets(0, 0, 0, 5);
        gbc_labelReplaceme.gridx = 1;
        gbc_labelReplaceme.gridy = 0;
        add(entityLinkToBrowser, gbc_labelReplaceme);
        
        panelControls = new JPanel(new BorderLayout());
        GridBagConstraints gbc_panelControls = new GridBagConstraints();
        gbc_panelControls.fill = GridBagConstraints.BOTH;
        gbc_panelControls.gridx = 2;
        gbc_panelControls.gridy = 0;
        add(panelControls, gbc_panelControls);
        panelControls.setLayout(new BorderLayout(0, 0));
        
        phasePanel = new JXPanel();
        panelControls.add(phasePanel);
        phasePanel.setBorder(null);
        phasePanel.setAlignmentX(1.0f);
        GridBagLayout gbl_phasePanel = new GridBagLayout();
        gbl_phasePanel.columnWidths = new int[]{88, 44, 51, 200};
        gbl_phasePanel.rowHeights = new int[]{16, 0};
        gbl_phasePanel.columnWeights = new double[]{0.0, 0.0, 0.0, 1.0};
        gbl_phasePanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
        phasePanel.setLayout(gbl_phasePanel);
        
        currentPhaseLabel = new JXLabel();
        currentPhaseLabel.setText("Current phase:");
        currentPhaseLabel.setFont(new Font("Tahoma", Font.BOLD, 13));
        currentPhaseLabel.setBorder(new EmptyBorder(0, 0, 0, 5));
        GridBagConstraints gbc_currentPhaseLabel = new GridBagConstraints();
        gbc_currentPhaseLabel.anchor = GridBagConstraints.WEST;
        gbc_currentPhaseLabel.insets = new Insets(0, 0, 0, 5);
        gbc_currentPhaseLabel.gridx = 0;
        gbc_currentPhaseLabel.gridy = 0;
        phasePanel.add(currentPhaseLabel, gbc_currentPhaseLabel);
        
        phaseDetails = new JXLabel();
        phaseDetails.setText("phase");
        phaseDetails.setFont(new Font("Tahoma", Font.PLAIN, 13));
        phaseDetails.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_phaseDetails = new GridBagConstraints();
        gbc_phaseDetails.anchor = GridBagConstraints.WEST;
        gbc_phaseDetails.insets = new Insets(0, 0, 0, 5);
        gbc_phaseDetails.gridx = 1;
        gbc_phaseDetails.gridy = 0;
        phasePanel.add(phaseDetails, gbc_phaseDetails);
        
        moveToLabel = new JXLabel();
        moveToLabel.setText("Move to:");
        moveToLabel.setFont(new Font("Tahoma", Font.BOLD, 13));
        moveToLabel.setBorder(new EmptyBorder(0, 0, 0, 5));
        GridBagConstraints gbc_moveToLabel = new GridBagConstraints();
        gbc_moveToLabel.anchor = GridBagConstraints.WEST;
        gbc_moveToLabel.insets = new Insets(0, 0, 0, 5);
        gbc_moveToLabel.gridx = 2;
        gbc_moveToLabel.gridy = 0;
        phasePanel.add(moveToLabel, gbc_moveToLabel);
        
        phaseComboBox = new PhaseComboBox();
        phaseComboBox.setPreferredSize(new Dimension(150, 30));
        phaseComboBox.setEditable(true);
        GridBagConstraints gbc_phaseComboBox = new GridBagConstraints();
        gbc_phaseComboBox.fill = GridBagConstraints.HORIZONTAL;
        gbc_phaseComboBox.gridx = 3;
        gbc_phaseComboBox.gridy = 0;
        phasePanel.add(phaseComboBox, gbc_phaseComboBox);
        
        
        buttonActionGroup = new DefaultActionGroup();
        ActionToolbar actionToolBar = ActionManager.getInstance().createActionToolbar("refresh | save", buttonActionGroup, true);
        panelControls.add(actionToolBar.getComponent(), BorderLayout.EAST);
    }

    public void setNameDetails(String nameDetails) {
        this.entityLinkToBrowser.setText(nameDetails);
    }

    public void setPhaseDetails(String phaseDetails) {
        this.phaseDetails.setText(phaseDetails);
    }

    public void setEntityIcon(ImageIcon entityIcon) {
    	entityIconLabel.setIcon(entityIcon);
    }

    public void setRefreshButton(AnAction refreshAction) {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(refreshAction);
    }

    public void setActionToEntityLink(Runnable runnable) {
        entityLinkToBrowser.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                runnable.run();
            }
        });
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
    	phaseComboBox.addItems(phasesList);
        if (phasesList.size() == 1) {
        	phaseComboBox.setEnabled(false);
        } else {
        	phaseComboBox.setEnabled(true);
        }
    }

    public EntityModel getSelectedTransition() {
        EntityModel selectedTransition = (EntityModel) phaseComboBox.getSelectedItem();
        return selectedTransition;
    }

    public void setSaveSelectedPhaseButton(AnAction saveSelectedPhaseAction) {
        buttonActionGroup.addSeparator();
        buttonActionGroup.add(saveSelectedPhaseAction);
        this.saveSelectedPhaseAction =saveSelectedPhaseAction;

    }
    public void removeSaveSelectedPhaseButton(){
        buttonActionGroup.remove(this.saveSelectedPhaseAction);
    }

    public void setPhaseInHeader(boolean showPhase){
        phasePanel.setVisible(showPhase);
    }

}
