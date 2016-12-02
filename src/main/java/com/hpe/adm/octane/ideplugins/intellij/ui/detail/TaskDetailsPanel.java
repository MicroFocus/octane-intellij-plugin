package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.JXTextArea;
import org.jdesktop.swingx.JXTextField;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import java.awt.*;

public class TaskDetailsPanel extends JPanel {
    private JXPanel detailsPanel;

    private JXLabel lblDescription;
    private JXTextArea txtfldDescription;

    private JXLabel lblType;
    private JXTextField txtfldType;

    private JXLabel lblPhase;
    private JComboBox comboBoxPhase;
    private JXLabel lblAuthor;
    private JXTextField txtfldAuthor;
    private JXLabel lblOwner;
    private JXTextField txtfldOwner;
    private JXLabel lblEstimatedHours;
    private JXTextField txtfldEstimatedHours;
    private JXLabel lblStory;
    private JXTextField txtfldStory;
    private JXLabel lblInvestedHours;
    private JXTextField textInvestedHours;
    private JXLabel lblLastModified;
    private JXLabel lblRemainingHours;
    private JXLabel lblCreationTime;
    private JXTextField txtfldRemainingHours;
    private JXTextField txtfldCreationTime;
    private JXTextField txtfldLastModified;
    private JXLabel lblName;

    public TaskDetailsPanel() {
        setBounds(100, 100, 918, 245);
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0};
        gridBagLayout.rowHeights = new int[]{0, 0};
        gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
        this.setLayout(gridBagLayout);

        JScrollPane scrollPane = new JScrollPane();
        GridBagConstraints gbc_scrollPane = new GridBagConstraints();
        gbc_scrollPane.fill = GridBagConstraints.BOTH;
        gbc_scrollPane.gridx = 0;
        gbc_scrollPane.gridy = 0;
        this.add(scrollPane, gbc_scrollPane);

        JXPanel parentPanel = new JXPanel();
        scrollPane.setViewportView(parentPanel);
        GridBagLayout gbl_parentPanel = new GridBagLayout();
        gbl_parentPanel.columnWidths = new int[]{0, 0};
        gbl_parentPanel.rowHeights = new int[]{0, 0, 0, 0, 0};
        gbl_parentPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
        gbl_parentPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
        parentPanel.setLayout(gbl_parentPanel);

        lblName = new JXLabel();
        lblName.setText("Name");
        lblName.setFont(new Font("Tahoma", Font.BOLD | Font.ITALIC, 13));
        lblName.setBorder(new EmptyBorder(5, 10, 0, 10));
        GridBagConstraints gbc_lblName = new GridBagConstraints();
        gbc_lblName.anchor = GridBagConstraints.WEST;
        gbc_lblName.insets = new Insets(0, 0, 5, 0);
        gbc_lblName.gridx = 0;
        gbc_lblName.gridy = 0;
        parentPanel.add(lblName, gbc_lblName);

        lblDescription = new JXLabel();
        lblDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        lblDescription.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblDescription.setText("Description");
        GridBagConstraints gbc_lblDescription = new GridBagConstraints();
        gbc_lblDescription.anchor = GridBagConstraints.NORTHWEST;
        gbc_lblDescription.insets = new Insets(0, 0, 5, 0);
        gbc_lblDescription.gridx = 0;
        gbc_lblDescription.gridy = 1;
        parentPanel.add(lblDescription, gbc_lblDescription);

        txtfldDescription = new JXTextArea();
        txtfldDescription.setEditable(false);
        txtfldDescription.setOpaque(false);
        txtfldDescription.setBackground(new Color(0, 0, 0, 0));
        txtfldDescription.setBorder(new EmptyBorder(5, 10, 0, 10));
        txtfldDescription.setLineWrap(true);
        txtfldDescription.setText("Description");
        GridBagConstraints gbc_txtrDescription = new GridBagConstraints();
        gbc_txtrDescription.insets = new Insets(0, 0, 5, 0);
        gbc_txtrDescription.fill = GridBagConstraints.BOTH;
        gbc_txtrDescription.gridx = 0;
        gbc_txtrDescription.gridy = 2;
        parentPanel.add(txtfldDescription, gbc_txtrDescription);

        detailsPanel = new JXPanel();
        detailsPanel.setBorder(new EmptyBorder(10, 10, 2, 10));
        GridBagConstraints gbc_detailsPanel = new GridBagConstraints();
        gbc_detailsPanel.fill = GridBagConstraints.BOTH;
        gbc_detailsPanel.gridx = 0;
        gbc_detailsPanel.gridy = 3;
        parentPanel.add(detailsPanel, gbc_detailsPanel);
        GridBagLayout gbl_detailsPanel = new GridBagLayout();
        gbl_detailsPanel.columnWidths = new int[]{0, 0, 0, 0, 0};
        gbl_detailsPanel.rowHeights = new int[]{0, 0, 0, 0, 0, 0, 0};
        gbl_detailsPanel.columnWeights = new double[]{0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
        gbl_detailsPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
        detailsPanel.setLayout(gbl_detailsPanel);

        lblStory = new JXLabel();
        lblStory.setText("Story");
        lblStory.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblStory.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblStory = new GridBagConstraints();
        gbc_lblStory.anchor = GridBagConstraints.WEST;
        gbc_lblStory.insets = new Insets(0, 0, 5, 5);
        gbc_lblStory.gridx = 0;
        gbc_lblStory.gridy = 0;
        detailsPanel.add(lblStory, gbc_lblStory);

        txtfldStory = new JXTextField();
        txtfldStory.setOpaque(false);
        txtfldStory.setBackground(new Color(0, 0, 0, 0));
        txtfldStory.setEditable(false);
        txtfldStory.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldStory.setBackground(new Color(0, 0, 0, 0));
        GridBagConstraints gbc_txtfldStory = new GridBagConstraints();
        gbc_txtfldStory.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldStory.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldStory.gridx = 1;
        gbc_txtfldStory.gridy = 0;
        detailsPanel.add(txtfldStory, gbc_txtfldStory);

        lblOwner = new JXLabel();
        lblOwner.setBorder(new EmptyBorder(0, 10, 0, 10));
        lblOwner.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblOwner.setText("Owner");
        GridBagConstraints gbc_lblOwner = new GridBagConstraints();
        gbc_lblOwner.anchor = GridBagConstraints.WEST;
        gbc_lblOwner.insets = new Insets(0, 0, 5, 5);
        gbc_lblOwner.gridx = 2;
        gbc_lblOwner.gridy = 0;
        detailsPanel.add(lblOwner, gbc_lblOwner);

        txtfldOwner = new JXTextField();
        txtfldOwner.setEditable(false);
        txtfldOwner.setOpaque(false);
        txtfldOwner.setBackground(new Color(0, 0, 0, 0));
        txtfldOwner.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldOwner = new GridBagConstraints();
        gbc_txtfldOwner.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldOwner.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldOwner.gridx = 3;
        gbc_txtfldOwner.gridy = 0;
        detailsPanel.add(txtfldOwner, gbc_txtfldOwner);

        lblAuthor = new JXLabel();
        lblAuthor.setText("Author");
        lblAuthor.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblAuthor.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblAuthor = new GridBagConstraints();
        gbc_lblAuthor.anchor = GridBagConstraints.WEST;
        gbc_lblAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_lblAuthor.gridx = 0;
        gbc_lblAuthor.gridy = 1;
        detailsPanel.add(lblAuthor, gbc_lblAuthor);

        txtfldAuthor = new JXTextField();
        txtfldAuthor.setEditable(false);
        txtfldAuthor.setOpaque(false);
        txtfldAuthor.setBackground(new Color(0, 0, 0, 0));
        txtfldAuthor.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldAuthor = new GridBagConstraints();
        gbc_txtfldAuthor.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldAuthor.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldAuthor.gridx = 1;
        gbc_txtfldAuthor.gridy = 1;
        detailsPanel.add(txtfldAuthor, gbc_txtfldAuthor);

        lblType = new JXLabel();
        lblType.setBorder(new EmptyBorder(0, 10, 0, 10));
        lblType.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblType.setText("Type");
        GridBagConstraints gbc_lblType = new GridBagConstraints();
        gbc_lblType.insets = new Insets(0, 0, 5, 5);
        gbc_lblType.anchor = GridBagConstraints.WEST;
        gbc_lblType.gridx = 2;
        gbc_lblType.gridy = 1;
        detailsPanel.add(lblType, gbc_lblType);

        txtfldType = new JXTextField();
        txtfldType.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        txtfldType.setEditable(false);
        txtfldType.setOpaque(false);
        txtfldType.setBackground(new Color(0, 0, 0, 0));

        GridBagConstraints gbc_txtfldType = new GridBagConstraints();
        gbc_txtfldType.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldType.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldType.gridx = 3;
        gbc_txtfldType.gridy = 1;
        detailsPanel.add(txtfldType, gbc_txtfldType);

        lblEstimatedHours = new JXLabel();
        lblEstimatedHours.setBorder(new EmptyBorder(0, 0, 0, 10));
        lblEstimatedHours.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblEstimatedHours.setText("Estimated hours");
        GridBagConstraints gbc_lblEstimatedHours = new GridBagConstraints();
        gbc_lblEstimatedHours.anchor = GridBagConstraints.WEST;
        gbc_lblEstimatedHours.insets = new Insets(0, 0, 5, 5);
        gbc_lblEstimatedHours.gridx = 0;
        gbc_lblEstimatedHours.gridy = 2;
        detailsPanel.add(lblEstimatedHours, gbc_lblEstimatedHours);

        txtfldEstimatedHours = new JXTextField();
        txtfldEstimatedHours.setEditable(false);
        txtfldEstimatedHours.setOpaque(false);
        txtfldEstimatedHours.setBackground(new Color(0, 0, 0, 0));
        txtfldEstimatedHours.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldEstimatedHours = new GridBagConstraints();
        gbc_txtfldEstimatedHours.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldEstimatedHours.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldEstimatedHours.gridx = 1;
        gbc_txtfldEstimatedHours.gridy = 2;
        detailsPanel.add(txtfldEstimatedHours, gbc_txtfldEstimatedHours);

        lblCreationTime = new JXLabel();
        lblCreationTime.setText("Creation Time");
        lblCreationTime.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblCreationTime.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblCreationTime = new GridBagConstraints();
        gbc_lblCreationTime.anchor = GridBagConstraints.WEST;
        gbc_lblCreationTime.insets = new Insets(0, 0, 5, 5);
        gbc_lblCreationTime.gridx = 2;
        gbc_lblCreationTime.gridy = 2;
        detailsPanel.add(lblCreationTime, gbc_lblCreationTime);

        txtfldCreationTime = new JXTextField();
        txtfldCreationTime.setEditable(false);
        txtfldCreationTime.setOpaque(false);
        txtfldCreationTime.setBackground(new Color(0, 0, 0, 0));
        txtfldCreationTime.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldCreationTime = new GridBagConstraints();
        gbc_txtfldCreationTime.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldCreationTime.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldCreationTime.gridx = 3;
        gbc_txtfldCreationTime.gridy = 2;
        detailsPanel.add(txtfldCreationTime, gbc_txtfldCreationTime);

        lblInvestedHours = new JXLabel();
        lblInvestedHours.setText("Invested hours");
        lblInvestedHours.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblInvestedHours.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblInvestedHours = new GridBagConstraints();
        gbc_lblInvestedHours.anchor = GridBagConstraints.WEST;
        gbc_lblInvestedHours.insets = new Insets(0, 0, 5, 5);
        gbc_lblInvestedHours.gridx = 0;
        gbc_lblInvestedHours.gridy = 3;
        detailsPanel.add(lblInvestedHours, gbc_lblInvestedHours);

        textInvestedHours = new JXTextField();
        textInvestedHours.setEditable(false);
        textInvestedHours.setOpaque(false);
        textInvestedHours.setBackground(new Color(0, 0, 0, 0));
        textInvestedHours.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_textInvestedHours = new GridBagConstraints();
        gbc_textInvestedHours.insets = new Insets(0, 0, 5, 5);
        gbc_textInvestedHours.fill = GridBagConstraints.HORIZONTAL;
        gbc_textInvestedHours.gridx = 1;
        gbc_textInvestedHours.gridy = 3;
        detailsPanel.add(textInvestedHours, gbc_textInvestedHours);

        lblLastModified = new JXLabel();
        lblLastModified.setText("Last modified");
        lblLastModified.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblLastModified.setBorder(new EmptyBorder(0, 10, 0, 10));
        GridBagConstraints gbc_lblLastModified = new GridBagConstraints();
        gbc_lblLastModified.anchor = GridBagConstraints.WEST;
        gbc_lblLastModified.insets = new Insets(0, 0, 5, 5);
        gbc_lblLastModified.gridx = 2;
        gbc_lblLastModified.gridy = 3;
        detailsPanel.add(lblLastModified, gbc_lblLastModified);

        txtfldLastModified = new JXTextField();
        txtfldLastModified.setEditable(false);
        txtfldLastModified.setOpaque(false);
        txtfldLastModified.setBackground(new Color(0, 0, 0, 0));
        txtfldLastModified.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldLastModified = new GridBagConstraints();
        gbc_txtfldLastModified.insets = new Insets(0, 0, 5, 0);
        gbc_txtfldLastModified.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldLastModified.gridx = 3;
        gbc_txtfldLastModified.gridy = 3;
        detailsPanel.add(txtfldLastModified, gbc_txtfldLastModified);

        lblRemainingHours = new JXLabel();
        lblRemainingHours.setText("Remaining Hours");
        lblRemainingHours.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblRemainingHours.setBorder(new EmptyBorder(0, 0, 0, 10));
        GridBagConstraints gbc_lblRemainingHours = new GridBagConstraints();
        gbc_lblRemainingHours.anchor = GridBagConstraints.WEST;
        gbc_lblRemainingHours.insets = new Insets(0, 0, 5, 5);
        gbc_lblRemainingHours.gridx = 0;
        gbc_lblRemainingHours.gridy = 4;
        detailsPanel.add(lblRemainingHours, gbc_lblRemainingHours);

        txtfldRemainingHours = new JXTextField();
        txtfldRemainingHours.setEditable(false);
        txtfldRemainingHours.setOpaque(false);
        txtfldRemainingHours.setBackground(new Color(0, 0, 0, 0));
        txtfldRemainingHours.setBorder(new MatteBorder(0, 0, 1, 0, (Color) new Color(0, 0, 0)));
        GridBagConstraints gbc_txtfldRemainingHours = new GridBagConstraints();
        gbc_txtfldRemainingHours.insets = new Insets(0, 0, 5, 5);
        gbc_txtfldRemainingHours.fill = GridBagConstraints.HORIZONTAL;
        gbc_txtfldRemainingHours.gridx = 1;
        gbc_txtfldRemainingHours.gridy = 4;
        detailsPanel.add(txtfldRemainingHours, gbc_txtfldRemainingHours);

        lblPhase = new JXLabel();
        lblPhase.setBorder(new EmptyBorder(0, 10, 0, 10));
        lblPhase.setFont(new Font("Tahoma", Font.BOLD, 11));
        lblPhase.setText("Phase");
        GridBagConstraints gbc_lblPhase = new GridBagConstraints();
        gbc_lblPhase.anchor = GridBagConstraints.WEST;
        gbc_lblPhase.insets = new Insets(0, 0, 5, 5);
        gbc_lblPhase.gridx = 2;
        gbc_lblPhase.gridy = 4;
        detailsPanel.add(lblPhase, gbc_lblPhase);

        comboBoxPhase = new JComboBox();
        comboBoxPhase.setEditable(true);
        comboBoxPhase.setBorder(new EmptyBorder(0, 0, 0, 0));
        comboBoxPhase.setModel(new DefaultComboBoxModel(new String[]{"Open", "In Progress", "Done"}));
        GridBagConstraints gbc_comboBox = new GridBagConstraints();
        gbc_comboBox.insets = new Insets(0, 0, 5, 0);
        gbc_comboBox.fill = GridBagConstraints.HORIZONTAL;
        gbc_comboBox.gridx = 3;
        gbc_comboBox.gridy = 4;
        detailsPanel.add(comboBoxPhase, gbc_comboBox);
    }

    public String getComboBoxPhase() {
        return comboBoxPhase.getSelectedItem().toString();
    }

    public void setComboBoxPhase(String phaseName) {
         this.comboBoxPhase.getEditor().setItem(phaseName);
    }

    public void setLblName(String lblName) {
        this.lblName.setText("Name: " + lblName);
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
