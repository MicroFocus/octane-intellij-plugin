/*******************************************************************************
 * Copyright 2017-2026 Open Text.
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
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import org.jdesktop.swingx.JXDatePicker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.text.NumberFormatter;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;

public class DateTimeFieldEditor extends FieldEditor {

    private static final Logger logger = LoggerFactory.getLogger(DateTimeFieldEditor.class.getName());

    protected EntityModelWrapper entityModelWrapper;
    protected String fieldName;

    private JLabel timeLabel;
    private JSpinner hourSpinner;
    private JSpinner minuteSpinner;
    private JSpinner secondsSpinner;

    private JBTextField dateTextField;
    private JXDatePicker datePickerComponent;

    private JLabel linkToButtons;
    private JLabel clearSelection;

    private DateTimeFormatter dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE;

    public DateTimeFieldEditor() {
        GridBagLayout gridBagLayout = new GridBagLayout();
        gridBagLayout.columnWidths = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        gridBagLayout.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
        setLayout(gridBagLayout);

        dateTextField = new JBTextField(10);
        dateTextField.setEditable(false);

        dateTextField.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                showDatePickerPopup();
            }
        });

        dateTextField.setCursor(new Cursor(Cursor.HAND_CURSOR));

        timeLabel = new JLabel("time :");

        SpinnerModel hourSpinnerModel = new SpinnerNumberModel(0, 0, 23, 1);
        hourSpinner = new JSpinner(hourSpinnerModel);
        JFormattedTextField hourSpinnerTextField = ((JSpinner.NumberEditor) hourSpinner.getEditor()).getTextField();
        ((NumberFormatter) hourSpinnerTextField.getFormatter()).setAllowsInvalid(false);
        hourSpinner.setPreferredSize(new Dimension(60, 30));

        SpinnerModel minuteSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        minuteSpinner = new JSpinner(minuteSpinnerModel);
        JFormattedTextField minuteSpinnerTextField = ((JSpinner.NumberEditor) minuteSpinner.getEditor()).getTextField();
        ((NumberFormatter) minuteSpinnerTextField.getFormatter()).setAllowsInvalid(false);
        minuteSpinner.setPreferredSize(new Dimension(60, 30));

        SpinnerModel secondsSpinnerModel = new SpinnerNumberModel(0, 0, 59, 1);
        secondsSpinner = new JSpinner(secondsSpinnerModel);
        JFormattedTextField secondsSpinnerTextField = ((JSpinner.NumberEditor) secondsSpinner.getEditor()).getTextField();
        ((NumberFormatter) secondsSpinnerTextField.getFormatter()).setAllowsInvalid(false);
        secondsSpinner.setPreferredSize(new Dimension(60, 30));

        linkToButtons = new JLabel("set date");
        linkToButtons.setForeground(UIManager.getColor("EditorPane.selectionBackground"));
        linkToButtons.setPreferredSize(new Dimension((int) linkToButtons.getPreferredSize().getWidth(), (int) hourSpinner.getPreferredSize().getHeight()));
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
    }

    private void showDatePickerPopup() {
        datePickerComponent = new JXDatePicker();

        if (dateTextField.getText() != null && !dateTextField.getText().isEmpty()) {
            try {
                LocalDate parsedDate = LocalDate.parse(dateTextField.getText(), dateFormatter);
                Calendar calendar = Calendar.getInstance();
                calendar.set(parsedDate.getYear(), parsedDate.getMonthValue() - 1, parsedDate.getDayOfMonth());
                datePickerComponent.setDate(calendar.getTime());
            } catch (Exception e) {
                datePickerComponent.setDate(new Date());
            }
        } else {
            datePickerComponent.setDate(new Date());
        }

        JBPopupFactory.getInstance()
                .createComponentPopupBuilder(datePickerComponent, datePickerComponent)
                .setMovable(true)
                .setResizable(true)
                .setTitle("Select Date")
                .setCancelCallback(() -> {
                    Date selectedDate = datePickerComponent.getDate();

                    if (selectedDate != null) {
                        LocalDate localDate = selectedDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
                        dateTextField.setText(dateFormatter.format(localDate));
                        handleChange();
                    }

                    return true;
                })
                .createPopup()
                .showUnderneathOf(dateTextField);
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
        addElementToPosition(dateTextField, 0);
        addElementToPosition(timeLabel, 1);
        addElementToPosition(hourSpinner, 2);
        addElementToPosition(new JLabel(":"), 3);
        addElementToPosition(minuteSpinner, 4);
        addElementToPosition(new JLabel(":"), 5);
        addElementToPosition(secondsSpinner, 6);

        GridBagConstraints gbc_emptyPlaceHolder = new GridBagConstraints();
        gbc_emptyPlaceHolder.anchor = GridBagConstraints.CENTER;
        gbc_emptyPlaceHolder.fill = GridBagConstraints.HORIZONTAL;
        gbc_emptyPlaceHolder.insets = JBUI.insetsRight(5);
        gbc_emptyPlaceHolder.gridx = 7;
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
            ZonedDateTime localZonedDateTime = timeStamp.atZone(ZoneId.systemDefault());

            dateTextField.setText(dateFormatter.format(localZonedDateTime.toLocalDate()));

            hourSpinner.setValue(localZonedDateTime.getHour());
            minuteSpinner.setValue(localZonedDateTime.getMinute());
            secondsSpinner.setValue(localZonedDateTime.getSecond());
            setDateTimeVisible();
        } else {
            dateTextField.setText("");

            hourSpinner.setValue(0);
            minuteSpinner.setValue(0);
            secondsSpinner.setValue(0);
        }
    }

    private ZonedDateTime getZonedDateTime() {
        if (dateTextField.getText().isEmpty()) {
            return null;
        }
        try {
            LocalDate localDate = LocalDate.parse(dateTextField.getText(), dateFormatter);
            int hour = Integer.parseInt(hourSpinner.getValue().toString());

            return ZonedDateTime.of(localDate,
                    LocalTime.of(hour,
                            Integer.parseInt(minuteSpinner.getValue().toString()),
                            Integer.parseInt(secondsSpinner.getValue().toString())),
                    ZoneId.systemDefault());
        } catch (DateTimeParseException e) {
            logger.error("Error parsing date/time: {}", e.getMessage());
            return null;
        }
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
            setLinkVisible();
        }
    }

    public Component getClearButton() {
        return clearSelection;
    }
}