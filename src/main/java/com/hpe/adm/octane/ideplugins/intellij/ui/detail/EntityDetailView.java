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
import java.util.Collection;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;

public class EntityDetailView implements View {

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

    public void setEntityModel(EntityModel entityModel) {
        entityDetailsPanel = new GeneralEntityDetailsPanel(entityModel, connectionSettingsProvider.getConnectionSettings());
        component.setViewportView(entityDetailsPanel);
    }

    public void setErrorMessage(String error) {
        JPanel errorPanel = new JPanel(new BorderLayout(0,0));

        JLabel errorLabel = new JLabel();
        errorLabel.setForeground(Color.RED);
        errorLabel.setText("<html>"+error+"</html>");
        errorPanel.add(errorLabel);
        errorLabel.setHorizontalAlignment(SwingConstants.CENTER);
        errorLabel.setVerticalAlignment(SwingConstants.CENTER);

        component.setViewportView(errorPanel);
    }

    public void setRefreshEntityButton(AnAction refreshAction) {
        entityDetailsPanel.drawRefreshButton(refreshAction);
    }

    public void doRefresh() {
        component.setViewportView(new JBScrollPane(new LoadingWidget()));
    }

    public void setPossiblePhasesForEntity(Collection<EntityModel> phasesList) {
        entityDetailsPanel.setPossiblePhasesForEntity(phasesList);
    }

}
