package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.intellij.execution.RunManager;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.Presentation;
import com.intellij.openapi.project.Project;


public class PhaseItemAction extends AnAction {
    private static final int SUMMARY_LENGHT = 10;
    private final RunnerAndConfigurationSettings myConfiguration;
    private final Project myProject;
    private String phaseName = " ";

    public PhaseItemAction(final RunnerAndConfigurationSettings configuration, final Project project) {
        this.myConfiguration = configuration;
        this.myProject = project;
        this.phaseName = configuration.getName();

        if (phaseName.length() > SUMMARY_LENGHT) {
            phaseName = phaseName.substring(0, SUMMARY_LENGHT) + "...";
        }
        getTemplatePresentation().setText(phaseName);
        final Presentation presentation = getTemplatePresentation();
        presentation.setText(phaseName, false);
        updateIcon(presentation);
    }

    private void updateIcon(final Presentation presentation) {
//        setConfigurationIcon(presentation, myConfiguration, myProject);
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        RunManager.getInstance(myProject).setSelectedConfiguration(myConfiguration);
//        updatePresentation(ExecutionTargetManager.getActiveTarget(myProject), myConfiguration, myProject, e.getPresentation());
    }

    @Override
    public void update(final AnActionEvent e) {
        super.update(e);
        updateIcon(e.getPresentation());
    }

}
