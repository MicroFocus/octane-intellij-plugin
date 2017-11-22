/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.LoadingWidget;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Set;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;

public class EntityDetailView implements View {

    private EntityModel entityModel;
    private JBScrollPane component = new JBScrollPane(new LoadingWidget());
    private GeneralEntityDetailsPanel entityDetailsPanel;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    public EntityDetailView() {

    }

    @Override
    public JComponent getComponent() {
        component.setBorder(null);
        component.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_ALWAYS);
        component.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);
        component.setMinimumSize(new Dimension(0, 0));
        return component;
    }


    public void createDetailsPanel(EntityModel entityModel, Set<String> fields) {
        this.entityModel = entityModel;
        entityDetailsPanel = new GeneralEntityDetailsPanel(entityModel, fields);
        component.setViewportView(entityDetailsPanel);
    }

    public void setErrorMessage(String error) {
        JPanel errorPanel = new JPanel(new BorderLayout(0, 0));

        JLabel errorLabel = new JLabel();
        errorLabel.setForeground(Color.RED);
        errorLabel.setText("<html><center>" + error + "</center></html>");
        errorPanel.add(errorLabel);
        errorLabel.setHorizontalAlignment(SwingConstants.CENTER);
        errorLabel.setVerticalAlignment(SwingConstants.CENTER);

        component.setViewportView(errorPanel);
    }

    public void setRefreshEntityButton(AnAction refreshAction) {
        entityDetailsPanel.setRefreshButton(refreshAction);
    }

    public void setCommentsEntityButton(AnAction commentsAction) {
        entityDetailsPanel.setCommentsButton(commentsAction);
    }

    public void setSaveSelectedPhaseButton(AnAction saveSelectedPhaseAction) {
        entityDetailsPanel.setSaveSelectedPhaseButton(saveSelectedPhaseAction);
    }

    public void removeSaveSelectedPhaseButton() {
        entityDetailsPanel.removeSaveSelectedPhaseButton();
    }

    public void setPhaseInHeader(boolean showPhase) {
        entityDetailsPanel.setPhaseInHeader(showPhase);
    }

    public void doRefresh() {
        component.setViewportView(new JBScrollPane(new LoadingWidget()));
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        entityDetailsPanel.setPossiblePhasesForEntity(phasesList);
    }

    public EntityModel getSelectedTransition() {
        return entityDetailsPanel.getSelectedTransition();
    }

    public EntityModel getEntityModel() {
        return this.entityModel;
    }

    public void setEntityNameClickHandler(Runnable runnable) {
        entityDetailsPanel.setEntityNameClickHandler(runnable);
    }

    public void setComments(Collection<EntityModel> comments) {
        entityDetailsPanel.setComments(comments);
    }

    public void addSendNewCommentAction(ActionListener actionListener) {
        entityDetailsPanel.addSendNewCommentAction(actionListener);
    }

    public void setFieldSelectButton(EntityDetailPresenter.SelectFieldsAction fieldSelectButton) {
        entityDetailsPanel.setFieldSelectButton(fieldSelectButton);
    }

    public void setCommentMessageBoxText(String t) {
        entityDetailsPanel.setCommentMessageBoxText(t);
    }

    public String getCommentMessageBoxText() {
        return entityDetailsPanel.getCommentMessageBoxText();
    }

    public GeneralEntityDetailsPanel getEntityDetailsPanel() {
        return entityDetailsPanel;
    }


}
