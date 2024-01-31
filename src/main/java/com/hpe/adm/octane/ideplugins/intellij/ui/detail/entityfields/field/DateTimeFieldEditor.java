/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields.field;

import com.hpe.adm.nga.sdk.model.DateFieldModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.nga.sdk.model.ReferenceFieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.services.model.EntityModelWrapper;
import com.intellij.openapi.util.IconLoader;
import com.intellij.util.ui.JBUI;
import com.michaelbaranov.microba.calendar.DatePicker;

import javax.swing.*;
import javax.swing.text.NumberFormatter;
import java.awt.*;
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
        microbaDatePicker.addActionListener(e -> handleChange());

        timeLabel = new JLabel("time :");

        SpinnerModel hourSpinnerModel = new SpinnerNumberModel(0, 0, 11, 1);
        hourSpinner = new JSpinner(hourSpinnerModel);
        JFormattedTextField hourSpinnerTextField = ((JSpinner.NumberEditor) hourSpinner.getEditor()).getTextField();
        ((NumberFormatter) hourSpinnerTextField.getFormatter()).setAllowsInvalid(false);
        hourSpinner.setPreferredSize(new Dimension(55,30));

        SpinnerModel minuteSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        minuteSpinner = new JSpinner(minuteSpinnerModel);
        JFormattedTextField minuteSpinnerTextField = ((JSpinner.NumberEditor) minuteSpinner.getEditor()).getTextField();
        ((NumberFormatter) minuteSpinnerTextField.getFormatter()).setAllowsInvalid(false);
        minuteSpinner.setPreferredSize(new Dimension(55,30));

        SpinnerModel secondsSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        secondsSpinner = new JSpinner(secondsSpinnerModel);
        JFormattedTextField secondsSpinnerTextField = ((JSpinner.NumberEditor) minuteSpinner.getEditor()).getTextField();
        ((NumberFormatter) secondsSpinnerTextField.getFormatter()).setAllowsInvalid(false);
        secondsSpinner.setPreferredSize(new Dimension(55,30));

        SpinnerModel daytimeSpinnerModel = new SpinnerListModel(Arrays.asList("AM", "PM"));
        dayTimeSpinner = new JSpinner(daytimeSpinnerModel);
        JTextField dayTimeSpinnerTextField = ((JSpinner.DefaultEditor) dayTimeSpinner.getEditor()).getTextField();
        dayTimeSpinnerTextField.setEditable(false);
        dayTimeSpinnerTextField.setColumns(3);
        dayTimeSpinnerTextField.setHorizontalAlignment(JTextField.CENTER);
        dayTimeSpinner.setPreferredSize(new Dimension(60,30));

        linkToButtons = new JLabel("set date");
        linkToButtons.setForeground(UIManager.getColor("EditorPane.selectionBackground"));
        linkToButtons.setPreferredSize(new Dimension((int) linkToButtons.getPreferredSize().getWidth(), (int) dayTimeSpinner.getPreferredSize().getHeight()));
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
        gbc_valueTextField.insets = JBUI.insetsRight(5);
        gbc_valueTextField.gridx = 0;
        gbc_valueTextField.weightx = 1.0;
        add(linkToButtons, gbc_valueTextField);

        clearSelection = new JLabel();
        clearSelection.setCursor(new Cursor(Cursor.HAND_CURSOR));
        clearSelection.setIcon(IconLoader.findIcon(Constants.getOctaneRemoveIcon(), DateTimeFieldEditor.class.getClassLoader()));
        clearSelection.setEnabled(false);

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

        hourSpinner.addChangeListener(e -> handleChange());
        minuteSpinner.addChangeListener(e -> handleChange());
        secondsSpinner.addChangeListener(e -> handleChange());
        dayTimeSpinner.addChangeListener(e -> handleChange());
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
        gbc.insets = JBUI.insetsRight(5);
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
        gbc_emptyPlaceHolder.insets = JBUI.insetsRight(5);
        gbc_emptyPlaceHolder.gridx = 8;
        gbc_emptyPlaceHolder.weightx = 1.0;
        add(new JLabel(), gbc_emptyPlaceHolder);

        clearSelection.setEnabled(true);
        repaint();
        revalidate();
    }

    private void setLinkVisible() {
        removeAll();
        clearSelection.setEnabled(false);
        GridBagConstraints gbc_linkToButtons = new GridBagConstraints();
        gbc_linkToButtons.anchor = GridBagConstraints.WEST;
        gbc_linkToButtons.fill = GridBagConstraints.HORIZONTAL;
        gbc_linkToButtons.insets = JBUI.insetsRight(5);
        gbc_linkToButtons.gridx = 0;
        gbc_linkToButtons.weightx = 1.0;
        add(linkToButtons, gbc_linkToButtons);
        repaint();
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

    public Component getClearButton() {
        return clearSelection;
    }
}
