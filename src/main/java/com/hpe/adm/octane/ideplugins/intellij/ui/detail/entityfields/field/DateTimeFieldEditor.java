package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.DateFieldModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.openapi.util.IconLoader;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Map;

public class DateTimeFieldEditor extends FieldEditor {


    protected EntityModelWrapper entityModelWrapper;
    protected String fieldName;

    private JLabel timeLabel;
    private JSpinner hourSpinner;
    private JSpinner minuteSpinner;
    private JSpinner secondsSpinner;
    private JSpinner dayTimeSpinner;

    private JLabel yearLabel;
    private JSpinner yearSpinner;
    private JLabel monthLabel;
    private JSpinner monthSpinner;
    private JLabel dayLabel;
    private JSpinner daySpinner;

    private JLabel linkToButtons;
    private JLabel clearSelection;

    public DateTimeFieldEditor() {

        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        setLayout(gridBagLayout);

        yearLabel = new JLabel("y :");
        monthLabel = new JLabel("m :");
        dayLabel = new JLabel("d :");

        SpinnerModel yearSpinnerModel = new SpinnerNumberModel(2018, 1950, 2100, 1);
        yearSpinner = new JSpinner(yearSpinnerModel);
        ((JSpinner.DefaultEditor) yearSpinner.getEditor()).getTextField().setEditable(false);

        SpinnerModel monthSpinnerModel = new SpinnerNumberModel(1, 1, 12, 1);
        monthSpinner = new JSpinner(monthSpinnerModel);
        ((JSpinner.DefaultEditor) monthSpinner.getEditor()).getTextField().setEditable(false);

        SpinnerModel daySpinnerModel = new SpinnerNumberModel(1, 1, 31, 1);
        daySpinner = new JSpinner(daySpinnerModel);
        ((JSpinner.DefaultEditor) daySpinner.getEditor()).getTextField().setEditable(false);

        timeLabel = new JLabel("time :");

        SpinnerModel hourSpinnerModel = new SpinnerNumberModel(0, 0, 11, 1);
        hourSpinner = new JSpinner(hourSpinnerModel);
        ((JSpinner.DefaultEditor) hourSpinner.getEditor()).getTextField().setEditable(false);

        SpinnerModel minuteSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        minuteSpinner = new JSpinner(minuteSpinnerModel);
        ((JSpinner.DefaultEditor) minuteSpinner.getEditor()).getTextField().setEditable(false);

        SpinnerModel secondsSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        secondsSpinner = new JSpinner(secondsSpinnerModel);
        ((JSpinner.DefaultEditor) secondsSpinner.getEditor()).getTextField().setEditable(false);

        SpinnerModel daytimeSpinnerModel = new SpinnerListModel(Arrays.asList("AM", "PM"));
        dayTimeSpinner = new JSpinner(daytimeSpinnerModel);
        ((JSpinner.DefaultEditor) dayTimeSpinner.getEditor()).getTextField().setEditable(false);

        linkToButtons = new JLabel("set date");
        linkToButtons.setForeground(UIManager.getColor("EditorPane.selectionBackground"));
        Font font = linkToButtons.getFont();
        Map attributes = font.getAttributes();
        attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
        linkToButtons.setFont(font.deriveFont(attributes));
        linkToButtons.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                setDateTimeVisible();
            }
        });
        GridBagConstraints gbc_valueTextField = new GridBagConstraints();
        gbc_valueTextField.anchor = GridBagConstraints.WEST;
        gbc_valueTextField.fill = GridBagConstraints.HORIZONTAL;
        gbc_valueTextField.insets = new Insets(0, 0, 0, 5);
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(linkToButtons, gbc_valueTextField);

        clearSelection = new JLabel();
        clearSelection.setIcon(IconLoader.findIcon(Constants.IMG_REMOVE_SELECTION));
//        btnSetNull.setImage(ImageResources.OCTANE_REMOVE.getImage());

        // Nullify
        clearSelection.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                setLinkVisible();
                entityModelWrapper.setValue(new ReferenceFieldModel(fieldName, null));
            }
        });

        // De-nullify
        linkToButtons.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                setDateTimeVisible();
                ZonedDateTime now = ZonedDateTime.now();
                setZonedDateTime(now);
                entityModelWrapper.setValue(new DateFieldModel(fieldName, now));
            }
        });

        yearSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                handleChange();
            }
        });

        monthSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                handleChange();
                changeDaySpinnerModel();
            }
        });

        daySpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                handleChange();
            }
        });

        hourSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                handleChange();
            }
        });
        minuteSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                handleChange();
            }
        });
        secondsSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                handleChange();
            }
        });
        dayTimeSpinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                handleChange();
            }
        });
    }

    private void handleChange() {
        entityModelWrapper.setValue(new DateFieldModel(fieldName, getZonedDateTime()));
    }

    private void changeDaySpinnerModel() {
        switch ((int) monthSpinner.getValue()) {
            case 2:
                daySpinner.setModel(new SpinnerNumberModel(1, 1, 28, 1));
                break;
            case 4:
            case 6:
            case 9:
            case 11:
                daySpinner.setModel(new SpinnerNumberModel(1, 1, 30, 1));
                break;
            default:
                daySpinner.setModel(new SpinnerNumberModel(1, 1, 31, 1));
        }
        ((JSpinner.DefaultEditor) daySpinner.getEditor()).getTextField().setEditable(false);
    }

    private void addElementToPosition(Component cmp, int x) {
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 0, 0, 5);
        gbc.gridx = x;
        add(cmp, gbc);
    }

    private void setDateTimeVisible() {
        removeAll();
        addElementToPosition(dayLabel, 0);
        addElementToPosition(daySpinner, 1);
        addElementToPosition(monthLabel, 2);
        addElementToPosition(monthSpinner, 3);
        addElementToPosition(yearLabel, 4);
        addElementToPosition(yearSpinner, 5);
        addElementToPosition(timeLabel, 6);
        addElementToPosition(hourSpinner, 7);
        addElementToPosition(minuteSpinner, 8);
        addElementToPosition(secondsSpinner, 9);
        addElementToPosition(dayTimeSpinner, 10);

        GridBagConstraints gbc_clearSelection = new GridBagConstraints();
        gbc_clearSelection.anchor = GridBagConstraints.CENTER;
        gbc_clearSelection.fill = GridBagConstraints.HORIZONTAL;
        gbc_clearSelection.insets = new Insets(0, 0, 0, 5);
        gbc_clearSelection.gridx = 11;
        gbc_clearSelection.weightx = 1.0;
        add(new JLabel(), gbc_clearSelection);

        addElementToPosition(clearSelection, 12);
        revalidate();
    }

    private void setLinkVisible() {
        removeAll();
        GridBagConstraints gbc_linkToButtons = new GridBagConstraints();
        gbc_linkToButtons.anchor = GridBagConstraints.WEST;
        gbc_linkToButtons.fill = GridBagConstraints.HORIZONTAL;
        gbc_linkToButtons.insets = new Insets(0, 0, 0, 5);
        gbc_linkToButtons.gridx = 0;
        gbc_linkToButtons.weightx = 1.0;
        add(linkToButtons, gbc_linkToButtons);
        revalidate();
    }

    private void setZonedDateTime(ZonedDateTime zonedDateTime) {

        if (zonedDateTime != null) {
            // Convert to local time for UI
            Instant timeStamp = zonedDateTime.toInstant();
            zonedDateTime = timeStamp.atZone(ZoneId.systemDefault());

            yearSpinner.setValue(zonedDateTime.getYear());
            monthSpinner.setValue(zonedDateTime.getMonthValue());
            daySpinner.setValue(zonedDateTime.getDayOfMonth());
            if (zonedDateTime.getHour() >= 12) {
                hourSpinner.setValue(zonedDateTime.getHour() - 12);
                dayTimeSpinner.setValue("PM");
            } else {
                hourSpinner.setValue(zonedDateTime.getHour());
                dayTimeSpinner.setValue("AM");
            }
            minuteSpinner.setValue(zonedDateTime.getMinute());
            secondsSpinner.setValue(zonedDateTime.getSecond());
            setDateTimeVisible();
        }
    }

    private ZonedDateTime getZonedDateTime() {

        // Converting to UTC is not necessary, the SDK will do it for you
        return ZonedDateTime.of(
                (int) yearSpinner.getValue(),
                (int) monthSpinner.getValue(),
                (int) daySpinner.getValue(),
                dayTimeSpinner.getValue().equals("AM") ? (int) hourSpinner.getValue() : (int) hourSpinner.getValue() + 12,
                (int) minuteSpinner.getValue(),
                (int) secondsSpinner.getValue(),
                0,
                ZoneId.systemDefault());

    }

    @Override
    public void setField(EntityModelWrapper entityModel, String fieldName) {
        this.entityModelWrapper = entityModel;
        this.fieldName = fieldName;

        FieldModel fieldModel = entityModel.getValue(fieldName);

        if (fieldModel != null && fieldModel.getValue() != null && fieldModel instanceof DateFieldModel) {
            setZonedDateTime((ZonedDateTime) fieldModel.getValue());
        } else {
            setZonedDateTime(null);
        }
    }
}
