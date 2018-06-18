package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Collection;
import java.util.Set;

import javax.swing.border.MatteBorder;

import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;

import com.hpe.adm.nga.sdk.metadata.FieldMetadata;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.services.util.Util;
import com.intellij.ui.JBColor;

@SuppressWarnings("serial")
public class EntityFieldsPanel extends JXPanel {

    private Collection<FieldMetadata> fields;
    private JXPanel detailsRightPanel;
    private JXPanel detailsLeftPanel;
    private JXLabel generalLabel;

    public EntityFieldsPanel(Collection<FieldMetadata> fields) {
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0, 0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, 1.0, 1.0, 1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
        setLayout(gridBagLayout);
        
        generalLabel = new JXLabel("General");
        generalLabel.setFont(new Font("Arial", Font.BOLD, 18));
        GridBagConstraints gbc_generalLabel = new GridBagConstraints();
        gbc_generalLabel.insets = new Insets(5, 0, 5, 0);
        gbc_generalLabel.gridwidth = 4;
        gbc_generalLabel.fill = GridBagConstraints.HORIZONTAL;
        gbc_generalLabel.gridx = 0;
        gbc_generalLabel.gridy = 0;
        add(generalLabel, gbc_generalLabel);
        
        detailsLeftPanel = new JXPanel();
        GridBagConstraints gbc_detailsLeftPanel = new GridBagConstraints();
        gbc_detailsLeftPanel.gridwidth = 2;
        gbc_detailsLeftPanel.insets = new Insets(0, 10, 5, 5);
        gbc_detailsLeftPanel.fill = GridBagConstraints.BOTH;
        gbc_detailsLeftPanel.gridx = 0;
        gbc_detailsLeftPanel.gridy = 1;
        add(detailsLeftPanel, gbc_detailsLeftPanel);
        GridBagLayout gbl_detailsLeftPanel = new GridBagLayout();
        detailsLeftPanel.setLayout(gbl_detailsLeftPanel);
        
        detailsRightPanel = new JXPanel();
        GridBagConstraints gbc_detailsRightPanel = new GridBagConstraints();
        gbc_detailsRightPanel.gridwidth = 2;
        gbc_detailsRightPanel.insets = new Insets(0, 5, 5, 0);
        gbc_detailsRightPanel.fill = GridBagConstraints.BOTH;
        gbc_detailsRightPanel.gridx = 2;
        gbc_detailsRightPanel.gridy = 1;
        add(detailsRightPanel, gbc_detailsRightPanel);
        GridBagLayout gbl_detailsRightPanel = new GridBagLayout();
        detailsRightPanel.setLayout(gbl_detailsRightPanel);
        
        this.fields = fields;
        addComponentListener(detailsLeftPanel, detailsRightPanel); 

    }
    
    public void addComponentListener(JXPanel detailsLeftPanel, JXPanel detailsRightPanel) {
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                int halfWidth = getWidth() / 2;
                int height = getHeight();
                if (halfWidth != 0 && height != 0) {
                    Dimension halfSizeFields =new Dimension((int) halfWidth, height);
                    detailsLeftPanel.setPreferredSize(halfSizeFields);
                    detailsRightPanel.setPreferredSize(halfSizeFields);
                    updateUI();
                    repaint();
                }
            }
        });
    }
    
    public void createSectionWithEntityDetails(EntityModel entityModel, Set<String> fieldNames) {
        detailsLeftPanel.removeAll();
        detailsRightPanel.removeAll();

        int fieldCount = 0;
        int i = 0;
        for (FieldMetadata fieldMetadata : fields) {
            if (fieldNames.contains(fieldMetadata.getName())) {
                String fieldName = fieldMetadata.getName();
                JXLabel fieldLabel = new JXLabel();
                fieldLabel.setFont(new Font("Arial", Font.BOLD, 12));
                fieldLabel.setText(fieldMetadata.getLabel());
                GridBagConstraints gbc1 = new GridBagConstraints();
                gbc1.anchor = GridBagConstraints.FIRST_LINE_START;
                gbc1.insets = new Insets(10, 0, 0, 0);
                gbc1.fill = GridBagConstraints.HORIZONTAL;
                gbc1.gridx = 0;
                gbc1.gridy = i;

                String fieldValue = null;

                if ("owner".equals(fieldName)
                        || "author".equals(fieldName)
                        || "run_by".equals(fieldName)
                        || "detected_by".equals(fieldName)) {
                    fieldValue = Util.getUiDataFromModel(entityModel.getValue(fieldName),
                            "full_name");
                } else {
                    fieldValue = Util.getUiDataFromModel(entityModel.getValue(fieldName));
                }
                JXLabel fieldValueLabel = new JXLabel();
                fieldValueLabel.setFont(new Font("Arial", Font.PLAIN, 12));
                fieldValueLabel.setText(fieldValue);
                fieldValueLabel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
                fieldValueLabel.setToolTipText(fieldValue);
                fieldValueLabel.setLineWrap(false);
                GridBagConstraints gbc2 = new GridBagConstraints();
                gbc2.insets = new Insets(10, 10, 0, 5);
                gbc2.anchor = GridBagConstraints.FIRST_LINE_START;
                gbc2.fill = GridBagConstraints.BOTH;
                gbc2.gridx = 1;
                gbc2.gridy = i;
                gbc2.weightx = 1.0;

                if (fieldCount % 2 == 0) {
                    detailsLeftPanel.add(fieldLabel, gbc1);
                    detailsLeftPanel.add(fieldValueLabel, gbc2);
                } else {
                    detailsRightPanel.add(fieldLabel, gbc1);
                    detailsRightPanel.add(fieldValueLabel, gbc2);
                }
                
                i++;
                fieldCount++;
            }
        }
        if (fieldCount % 2==1) {
            JXLabel emptyLabel = new JXLabel();
            emptyLabel.setFont(new Font("Arial", Font.BOLD, 12));
            GridBagConstraints gbc = new GridBagConstraints();
            gbc.insets = new Insets(20, 10, 0, 5);
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.fill = GridBagConstraints.BOTH;
            gbc.gridx = 0;
            gbc.gridy = i;
            gbc.gridwidth = 2;
            detailsRightPanel.add(emptyLabel, gbc);
        }
        detailsLeftPanel.repaint();
        detailsLeftPanel.revalidate();
        detailsRightPanel.repaint();
        detailsRightPanel.revalidate();
    }
}
