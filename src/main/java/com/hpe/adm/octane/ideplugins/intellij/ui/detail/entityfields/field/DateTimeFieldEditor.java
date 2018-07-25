package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.DateFieldModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.openapi.util.IconLoader;
import com.michaelbaranov.microba.calendar.DatePicker;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.NumberFormatter;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.beans.PropertyVetoException;
import java.time.*;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Map;

public class DateTimeFieldEditor extends FieldEditor {


    protected EntityModelWrapper entityModelWrapper;
    protected String fieldName;

    private JLabel timeLabel;
    private JSpinner hourSpinner;
    private JSpinner minuteSpinner;
    private JSpinner secondsSpinner;
    private JSpinner dayTimeSpinner;

    private DatePicker microbaDatePicker;

    private JLabel linkToButtons;
    private JLabel clearSelection;


    public DateTimeFieldEditor() {

        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        setLayout(gridBagLayout);

        microbaDatePicker = new DatePicker();
        microbaDatePicker.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                handleChange();
            }
        });


        timeLabel = new JLabel("time :");

        SpinnerModel hourSpinnerModel = new SpinnerNumberModel(0, 0, 11, 1);
        hourSpinner = new JSpinner(hourSpinnerModel);
        JFormattedTextField hourSpinnerTextField = ((JSpinner.NumberEditor) hourSpinner.getEditor()).getTextField();
        ((NumberFormatter) hourSpinnerTextField.getFormatter()).setAllowsInvalid(false);

        SpinnerModel minuteSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        minuteSpinner = new JSpinner(minuteSpinnerModel);
        JFormattedTextField minuteSpinnerTextField = ((JSpinner.NumberEditor) minuteSpinner.getEditor()).getTextField();
        ((NumberFormatter) minuteSpinnerTextField.getFormatter()).setAllowsInvalid(false);

        SpinnerModel secondsSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        secondsSpinner = new JSpinner(secondsSpinnerModel);
        JFormattedTextField secondsSpinnerTextField = ((JSpinner.NumberEditor) minuteSpinner.getEditor()).getTextField();
        ((NumberFormatter) secondsSpinnerTextField.getFormatter()).setAllowsInvalid(false);

        SpinnerModel daytimeSpinnerModel = new SpinnerListModel(Arrays.asList("AM", "PM"));
        dayTimeSpinner = new JSpinner(daytimeSpinnerModel);
        JTextField dayTimeSpinnerTextField = ((JSpinner.DefaultEditor) dayTimeSpinner.getEditor()).getTextField();
        dayTimeSpinnerTextField.setEditable(false);
        dayTimeSpinnerTextField.setColumns(3);
        dayTimeSpinnerTextField.setHorizontalAlignment(JTextField.CENTER);

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
        gbc_valueTextField.insets = new Insets(0, 5, 0, 5);
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(linkToButtons, gbc_valueTextField);

        clearSelection = new JLabel();
        clearSelection.setIcon(IconLoader.findIcon(Constants.IMG_REMOVE_SELECTION));

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
        ZonedDateTime zdt = getZonedDateTime();
        if (zdt == null) {
            entityModelWrapper.setValue(new ReferenceFieldModel(fieldName, null));
        } else {
            entityModelWrapper.setValue(new DateFieldModel(fieldName, getZonedDateTime()));
        }
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
        addElementToPosition(microbaDatePicker, 0);
        addElementToPosition(timeLabel, 1);
        addElementToPosition(hourSpinner, 2);
        addElementToPosition(new JLabel(":"), 3);
        addElementToPosition(minuteSpinner, 4);
        addElementToPosition(new JLabel(":"), 5);
        addElementToPosition(secondsSpinner, 6);
        addElementToPosition(dayTimeSpinner, 7);

        GridBagConstraints gbc_emptyPlaceHolder = new GridBagConstraints();
        gbc_emptyPlaceHolder.anchor = GridBagConstraints.CENTER;
        gbc_emptyPlaceHolder.fill = GridBagConstraints.HORIZONTAL;
        gbc_emptyPlaceHolder.insets = new Insets(0, 0, 0, 5);
        gbc_emptyPlaceHolder.gridx = 8;
        gbc_emptyPlaceHolder.weightx = 1.0;
        add(new JLabel(), gbc_emptyPlaceHolder);

        addElementToPosition(clearSelection, 9);
        revalidate();
    }

    private void setLinkVisible() {
        removeAll();
        GridBagConstraints gbc_linkToButtons = new GridBagConstraints();
        gbc_linkToButtons.anchor = GridBagConstraints.WEST;
        gbc_linkToButtons.fill = GridBagConstraints.HORIZONTAL;
        gbc_linkToButtons.insets = new Insets(0, 5, 0, 5);
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

            Calendar calendar = Calendar.getInstance();
            calendar.set(Calendar.YEAR, zonedDateTime.getYear());
            //Date uses 0 based numbering while ZonedDateTime uses 1 based numbering
            calendar.set(Calendar.MONTH, zonedDateTime.getMonthValue() - 1);
            calendar.set(Calendar.DATE, zonedDateTime.getDayOfMonth());

            try {
                microbaDatePicker.setDate(calendar.getTime());
            } catch (PropertyVetoException e) {
            }

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
        if (microbaDatePicker.getDate() == null) {
            //user might click on none button in the date picker
            return null;
        }
        LocalDate localDate = microbaDatePicker.getDate()
                .toInstant()
                .atZone(ZoneId.systemDefault()).toLocalDate();
        //Date uses 0 based numbering while ZonedDateTime uses 1 based numbering
        localDate.plusMonths(1);
        // Converting to UTC is not necessary, the SDK will do it for you
        return ZonedDateTime.of(localDate,
                LocalTime.of(
                        dayTimeSpinner.getValue().equals("AM") ? (int) hourSpinner.getValue() : (int) hourSpinner.getValue() + 12,
                        (int) minuteSpinner.getValue(),
                        (int) secondsSpinner.getValue()),
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
