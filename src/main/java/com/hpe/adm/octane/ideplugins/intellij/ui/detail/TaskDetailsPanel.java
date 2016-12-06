package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.ui.JBColor;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

public class TaskDetailsPanel extends JPanel {
    private JXPanel rootPanel;
    private JXLabel lblDescription;
    private JXTextArea txtfldDescription;
    private JXPanel taskDetailsPanel;
    private JXLabel lblStory;
    private JXTextField txtfldStory;
    private JXLabel lblOwner;
    private JXTextField txtfldOwner;
    private JXLabel lblAuthor;
    private JXTextField txtfldAuthor;
    private JXLabel lblType;
    private JXTextField txtfldType;
    private JXLabel lblEstimatedHours;
    private JXTextField txtfldEstimatedHours;
    private JXLabel lblCreationTime;
    private JXTextField txtfldCreationTime;
    private JXLabel lblInvestedHours;
    private JXTextField textInvestedHours;
    private JXLabel lblLastModified;
    private JXTextField txtfldLastModified;
    private JXLabel lblRemainingHours;
    private JXTextField txtfldRemainingHours;
    private JXLabel lblPhase;
    private JComboBox comboBoxPhase;
    private JXPanel nameAndIconPanel;
    private JXLabel lblName;

    public TaskDetailsPanel() {
        setBorder(null);
        setBounds(100, 100, 900, 300);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
        this.setLayout(gridBagLayout);

        rootPanel = new JXPanel();
        rootPanel.setBorder(null);
        GridBagConstraints gbc_rootPanel = new GridBagConstraints();
        gbc_rootPanel.fill = GridBagConstraints.BOTH;
        gbc_rootPanel.gridx = 0;
        gbc_rootPanel.gridy = 0;
        add(rootPanel, gbc_rootPanel);
        GridBagLayout gbl_rootPanel = new GridBagLayout();
        gbl_rootPanel.columnWidths = new int[]{0, 0};
        gbl_rootPanel.rowHeights = new int[]{0, 0, 0, 0, 0};
        gbl_rootPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_rootPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        rootPanel.setLayout(gbl_rootPanel);

        nameAndIconPanel = new JXPanel();
        nameAndIconPanel.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        FlowLayout flowLayout = (FlowLayout) nameAndIconPanel.getLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        GridBagConstraints gbc_nameAndIconPanel = new GridBagConstraints();
        gbc_nameAndIconPanel.insets = new Insets(0, 0, 5, 0);
        gbc_nameAndIconPanel.fill = GridBagConstraints.BOTH;
        gbc_nameAndIconPanel.gridx = 0;
        gbc_nameAndIconPanel.gridy = 0;
        rootPanel.add(nameAndIconPanel, gbc_nameAndIconPanel);
        
        lblName = new JXLabel();
        lblName.setIcon(new ImageIcon(TaskDetailsPanel.class.getResource("/images/taskIcon.png")));
        lblName.setText("Name");
        lblName.setFont(new Font("Tahoma", Font.BOLD, 12));
        lblName.setBorder(null);
        nameAndIconPanel.add(lblName);
        
        lblDescription = new JXLabel();
        lblDescription.setText("Description");
        lblDescription.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        GridBagConstraints gbc_lblDescription = new GridBagConstraints();
        gbc_lblDescription.anchor = GridBagConstraints.NORTHWEST;
        gbc_lblDescription.insets = new Insets(0, 0, 5, 0);
        gbc_lblDescription.gridx = 0;
        gbc_lblDescription.gridy = 1;
        rootPanel.add(lblDescription, gbc_lblDescription);
        
        txtfldDescription = new JXTextArea();
        txtfldDescription.setText("Description");
        txtfldDescription.setOpaque(false);
        txtfldDescription.setLineWrap(true);
        txtfldDescription.setEditable(false);
        txtfldDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        txtfldDescription.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldDescription = new GridBagConstraints();
        gbc_txtfldDescription.fill = GridBagConstraints.BOTH;
        gbc_txtfldDescription.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldDescription.gridx = 0;
        gbc_txtfldDescription.gridy = 2;
        rootPanel.add(txtfldDescription, gbc_txtfldDescription);

        taskDetailsPanel = new JXPanel();
        taskDetailsPanel.setBorder(new EmptyBorder(10, 10, 2, 10));
        GridBagConstraints gbc_taskDetailsPanel = new GridBagConstraints();
        gbc_taskDetailsPanel.fill = GridBagConstraints.BOTH;
        gbc_taskDetailsPanel.gridx = 0;
        gbc_taskDetailsPanel.gridy = 3;
        rootPanel.add(taskDetailsPanel, gbc_taskDetailsPanel);
        GridBagLayout gbl_taskDetailsPanel = new GridBagLayout();
        gbl_taskDetailsPanel.columnWidths = new int[]{0, 0, 0, 0, 0};
        gbl_taskDetailsPanel.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0};
        gbl_taskDetailsPanel.columnWeights = new double[]{0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_taskDetailsPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        taskDetailsPanel.setLayout(gbl_taskDetailsPanel);
        
        lblStory = new JXLabel();
        lblStory.setText("Story");
        lblStory.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblStory.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblStory = new GridBagConstraints();
        gbc_lblStory.anchor = GridBagConstraints.WEST;
        gbc_lblStory.insets = new Insets(0, 0, 5, 5);
        gbc_lblStory.gridx = 0;
        gbc_lblStory.gridy = 0;
        taskDetailsPanel.add(lblStory, gbc_lblStory);
        
        txtfldStory = new JXTextField();
        txtfldStory.setOpaque(false);
        txtfldStory.setEditable(false);
        txtfldStory.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldStory.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldStory = new GridBagConstraints();
        gbc_txtfldStory.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldStory.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldStory.gridx = 1;
        gbc_txtfldStory.gridy = 0;
        taskDetailsPanel.add(txtfldStory, gbc_txtfldStory);
        
        lblOwner = new JXLabel();
        lblOwner.setText("Owner");
        lblOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblOwner.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblOwner = new GridBagConstraints();
        gbc_lblOwner.anchor = GridBagConstraints.WEST;
        gbc_lblOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblOwner.gridx = 2;
        gbc_lblOwner.gridy = 0;
        taskDetailsPanel.add(lblOwner, gbc_lblOwner);
        
        txtfldOwner = new JXTextField();
        txtfldOwner.setOpaque(false);
        txtfldOwner.setEditable(false);
        txtfldOwner.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldOwner.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldOwner = new GridBagConstraints();
        gbc_txtfldOwner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldOwner.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldOwner.gridx = 3;
        gbc_txtfldOwner.gridy = 0;
        taskDetailsPanel.add(txtfldOwner, gbc_txtfldOwner);
        
        lblAuthor = new JXLabel();
        lblAuthor.setText("Author");
        lblAuthor.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblAuthor.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblAuthor = new GridBagConstraints();
        gbc_lblAuthor.anchor = GridBagConstraints.WEST;
        gbc_lblAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_lblAuthor.gridx = 0;
        gbc_lblAuthor.gridy = 1;
        taskDetailsPanel.add(lblAuthor, gbc_lblAuthor);
        
        txtfldAuthor = new JXTextField();
        txtfldAuthor.setOpaque(false);
        txtfldAuthor.setEditable(false);
        txtfldAuthor.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldAuthor.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldAuthor = new GridBagConstraints();
        gbc_txtfldAuthor.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldAuthor.gridx = 1;
        gbc_txtfldAuthor.gridy = 1;
        taskDetailsPanel.add(txtfldAuthor, gbc_txtfldAuthor);
        
        lblType = new JXLabel();
        lblType.setText("Type");
        lblType.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblType.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblType = new GridBagConstraints();
        gbc_lblType.anchor = GridBagConstraints.WEST;
        gbc_lblType.insets = new Insets(0, 0, 5, 5);
        gbc_lblType.gridx = 2;
        gbc_lblType.gridy = 1;
        taskDetailsPanel.add(lblType, gbc_lblType);
        
        txtfldType = new JXTextField();
        txtfldType.setOpaque(false);
        txtfldType.setEditable(false);
        txtfldType.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldType.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldType = new GridBagConstraints();
        gbc_txtfldType.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldType.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldType.gridx = 3;
        gbc_txtfldType.gridy = 1;
        taskDetailsPanel.add(txtfldType, gbc_txtfldType);
        
        lblEstimatedHours = new JXLabel();
        lblEstimatedHours.setText("Estimated hours");
        lblEstimatedHours.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblEstimatedHours.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblEstimatedHours = new GridBagConstraints();
        gbc_lblEstimatedHours.anchor = GridBagConstraints.WEST;
        gbc_lblEstimatedHours.insets = new Insets(0, 0, 5, 5);
        gbc_lblEstimatedHours.gridx = 0;
        gbc_lblEstimatedHours.gridy = 2;
        taskDetailsPanel.add(lblEstimatedHours, gbc_lblEstimatedHours);
        
        txtfldEstimatedHours = new JXTextField();
        txtfldEstimatedHours.setOpaque(false);
        txtfldEstimatedHours.setEditable(false);
        txtfldEstimatedHours.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldEstimatedHours.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldEstimatedHours = new GridBagConstraints();
        gbc_txtfldEstimatedHours.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldEstimatedHours.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldEstimatedHours.gridx = 1;
        gbc_txtfldEstimatedHours.gridy = 2;
        taskDetailsPanel.add(txtfldEstimatedHours, gbc_txtfldEstimatedHours);
        
        lblCreationTime = new JXLabel();
        lblCreationTime.setText("Creation Time");
        lblCreationTime.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblCreationTime.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblCreationTime = new GridBagConstraints();
        gbc_lblCreationTime.anchor = GridBagConstraints.WEST;
        gbc_lblCreationTime.insets = new Insets(0, 0, 5, 5);
        gbc_lblCreationTime.gridx = 2;
        gbc_lblCreationTime.gridy = 2;
        taskDetailsPanel.add(lblCreationTime, gbc_lblCreationTime);
        
        txtfldCreationTime = new JXTextField();
        txtfldCreationTime.setOpaque(false);
        txtfldCreationTime.setEditable(false);
        txtfldCreationTime.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldCreationTime.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldCreationTime = new GridBagConstraints();
        gbc_txtfldCreationTime.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldCreationTime.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldCreationTime.gridx = 3;
        gbc_txtfldCreationTime.gridy = 2;
        taskDetailsPanel.add(txtfldCreationTime, gbc_txtfldCreationTime);
        
        lblInvestedHours = new JXLabel();
        lblInvestedHours.setText("Invested hours");
        lblInvestedHours.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblInvestedHours.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblInvestedHours = new GridBagConstraints();
        gbc_lblInvestedHours.anchor = GridBagConstraints.WEST;
        gbc_lblInvestedHours.insets = new Insets(0, 0, 5, 5);
        gbc_lblInvestedHours.gridx = 0;
        gbc_lblInvestedHours.gridy = 3;
        taskDetailsPanel.add(lblInvestedHours, gbc_lblInvestedHours);
        
        textInvestedHours = new JXTextField();
        textInvestedHours.setOpaque(false);
        textInvestedHours.setEditable(false);
        textInvestedHours.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        textInvestedHours.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_textInvestedHours = new GridBagConstraints();
        gbc_textInvestedHours.fill = GridBagConstraints.HORIZONTAL;
        gbc_textInvestedHours.insets = new Insets(0, 0, 5, 5);
        gbc_textInvestedHours.gridx = 1;
        gbc_textInvestedHours.gridy = 3;
        taskDetailsPanel.add(textInvestedHours, gbc_textInvestedHours);
        
        lblLastModified = new JXLabel();
        lblLastModified.setText("Last modified");
        lblLastModified.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblLastModified.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblLastModified = new GridBagConstraints();
        gbc_lblLastModified.anchor = GridBagConstraints.WEST;
        gbc_lblLastModified.insets = new Insets(0, 0, 5, 5);
        gbc_lblLastModified.gridx = 2;
        gbc_lblLastModified.gridy = 3;
        taskDetailsPanel.add(lblLastModified, gbc_lblLastModified);
        
        txtfldLastModified = new JXTextField();
        txtfldLastModified.setOpaque(false);
        txtfldLastModified.setEditable(false);
        txtfldLastModified.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldLastModified.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldLastModified = new GridBagConstraints();
        gbc_txtfldLastModified.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldLastModified.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldLastModified.gridx = 3;
        gbc_txtfldLastModified.gridy = 3;
        taskDetailsPanel.add(txtfldLastModified, gbc_txtfldLastModified);
        
        lblRemainingHours = new JXLabel();
        lblRemainingHours.setText("Remaining Hours");
        lblRemainingHours.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRemainingHours.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblRemainingHours = new GridBagConstraints();
        gbc_lblRemainingHours.anchor = GridBagConstraints.WEST;
        gbc_lblRemainingHours.insets = new Insets(0, 0, 5, 5);
        gbc_lblRemainingHours.gridx = 0;
        gbc_lblRemainingHours.gridy = 4;
        taskDetailsPanel.add(lblRemainingHours, gbc_lblRemainingHours);
        
        txtfldRemainingHours = new JXTextField();
        txtfldRemainingHours.setOpaque(false);
        txtfldRemainingHours.setEditable(false);
        txtfldRemainingHours.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()));
        txtfldRemainingHours.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldRemainingHours = new GridBagConstraints();
        gbc_txtfldRemainingHours.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldRemainingHours.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldRemainingHours.gridx = 1;
        gbc_txtfldRemainingHours.gridy = 4;
        taskDetailsPanel.add(txtfldRemainingHours, gbc_txtfldRemainingHours);
        
        lblPhase = new JXLabel();
        lblPhase.setText("Phase");
        lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblPhase.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblPhase = new GridBagConstraints();
        gbc_lblPhase.anchor = GridBagConstraints.WEST;
        gbc_lblPhase.insets = new Insets(0, 0, 5, 5);
        gbc_lblPhase.gridx = 2;
        gbc_lblPhase.gridy = 4;
        taskDetailsPanel.add(lblPhase, gbc_lblPhase);
        
        comboBoxPhase = new JComboBox();
        comboBoxPhase.setEditable(true);
        comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
        GridBagConstraints gbc_comboBoxPhase = new GridBagConstraints();
        gbc_comboBoxPhase.fill = GridBagConstraints.HORIZONTAL;
        gbc_comboBoxPhase.insets = new Insets(0, 0, 5, 0);
        gbc_comboBoxPhase.gridx = 3;
        gbc_comboBoxPhase.gridy = 4;
        taskDetailsPanel.add(comboBoxPhase, gbc_comboBoxPhase);
    }

    public String getComboBoxPhase() {
        return comboBoxPhase.getSelectedItem().toString();
    }

    public void setComboBoxPhase(String phaseName) {
         this.comboBoxPhase.getEditor().setItem(phaseName);
    }

    public void setLblName(String lblName) {
        this.lblName.setText(lblName);
    }

    public void setTxtfldDescription(String txtfldDescription) {
        this.txtfldDescription.setText(txtfldDescription);
    }

    public void setTxtfldType(String txtfldType) {
        this.txtfldType.setText(txtfldType);
    }

    public void setTxtfldAuthor(String txtfldAuthor) {
        this.txtfldAuthor.setText(txtfldAuthor);
    }

    public void setTxtfldOwner(String txtfldOwner) {
        this.txtfldOwner.setText(txtfldOwner);
    }

    public void setTxtfldEstimatedHours(String txtfldEstimatedHours) {
        this.txtfldEstimatedHours.setText(txtfldEstimatedHours);
    }

    public void setTxtfldStory(String txtfldStory) {
        this.txtfldStory.setText(txtfldStory);
    }

    public void setTextInvestedHours(String textInvestedHours) {
        this.textInvestedHours.setText(textInvestedHours);
    }

    public void setTxtfldRemainingHours(String txtfldRemainingHours) {
        this.txtfldRemainingHours.setText(txtfldRemainingHours);
    }

    public void setTxtfldCreationTime(String txtfldCreationTime) {
        this.txtfldCreationTime.setText(txtfldCreationTime);
    }

    public void setTxtfldLastModified(String txtfldLastModified) {
        this.txtfldLastModified.setText(txtfldLastModified);
    }


}
