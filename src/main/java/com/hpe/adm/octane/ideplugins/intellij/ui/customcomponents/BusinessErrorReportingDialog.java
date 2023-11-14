/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.hpe.adm.nga.sdk.exception.OctaneException;
import com.hpe.adm.nga.sdk.model.ErrorModel;
import com.hpe.adm.nga.sdk.model.FieldModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.VerticalFlowLayout;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;

public class BusinessErrorReportingDialog extends DialogWrapper {

    public static final String ERORR_MODEL_FIELD_DESCRIPTION_TRANSLATED = "description_translated";
    public static final String ERORR_MODEL_FIELD_DESCRIPTION = "description";
    public static final String ERORR_MODEL_FIELD_PROPERTIES = "properties";
    public static final String ERROR_MODEL_FIELD_STACK_TRACE = "stack_trace";

    public static final int EXIT_CODE_BACK = 1;
    public static final int EXIT_CODE_REFRESH = 2;
    public static final int EXIT_CODE_OPEN_IN_BROWSER = 3;

    private OctaneException ex;

    private Action[] actions;

    public BusinessErrorReportingDialog(@Nullable Project project, OctaneException ex) {
        super(project);
        this.ex = ex;
        setTitle("Business Rule Violation");
        init();
        setResizable(false);
    }


    @Nullable
    @Override
    protected JComponent createCenterPanel() {
        ErrorModel errorModel = ex.getError();

        JPanel rootPanel = new JPanel(new BorderLayout());

        JPanel leftPanel = new JPanel(new VerticalFlowLayout());
        JPanel rightPanel = new JPanel(new VerticalFlowLayout());

        JLabel errorMessage = new JLabel(errorModel.getValue(ERORR_MODEL_FIELD_DESCRIPTION_TRANSLATED).getValue().toString());
        errorMessage.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
        errorMessage.setForeground(Color.RED);
        errorModel.removeValue(ERORR_MODEL_FIELD_DESCRIPTION_TRANSLATED);
        errorModel.removeValue(ERORR_MODEL_FIELD_DESCRIPTION);
        errorModel.removeValue(ERORR_MODEL_FIELD_PROPERTIES);

        rootPanel.add(errorMessage, BorderLayout.NORTH);

        Object fieldValueStackTrace = errorModel.getValue(ERROR_MODEL_FIELD_STACK_TRACE);
        errorModel.removeValue(ERROR_MODEL_FIELD_STACK_TRACE);

        ex.getError().getValues().forEach(f -> {
            //display the messages as name - value pairs
            String fieldName = UiUtil.convertFieldNameToLabel(f.getName());
            leftPanel.add(new JLabel(fieldName));
            rightPanel.add(new JLabel(f.getValue().toString()));

        });

        if (fieldValueStackTrace != null) {
            leftPanel.add(new JLabel(UiUtil.convertFieldNameToLabel(ERROR_MODEL_FIELD_STACK_TRACE)));
            JTextArea textArea = new JTextArea(10, 75);
            textArea.setEditable(false);
            textArea.setText(((FieldModel) fieldValueStackTrace).getValue().toString());
            JScrollPane sp = new JScrollPane(textArea);
            rightPanel.add(sp);
        }

        rootPanel.add(leftPanel, BorderLayout.WEST);
        rootPanel.add(rightPanel, BorderLayout.CENTER);
        return rootPanel;
    }

    @Override
    protected Action[] createActions() {
        actions = new Action[]{new DialogWrapperExitAction("Back", EXIT_CODE_BACK),
                new DialogWrapperExitAction("Refresh", EXIT_CODE_REFRESH),
                new DialogWrapperExitAction("Open in browser", EXIT_CODE_OPEN_IN_BROWSER)};
        return actions;
    }

    @Override
    public JComponent getPreferredFocusedComponent() {
        return getButton(actions[0]);
    }
}
