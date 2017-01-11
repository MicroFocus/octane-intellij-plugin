package com.hpe.adm.octane.ideplugins.intellij.ui.detail.actions;

import com.intellij.execution.RunManagerEx;
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.actionSystem.ex.ComboBoxAction;
import com.intellij.openapi.project.IndexNotReadyException;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public final class PhaseComboBoxAction extends ComboBoxAction {
    public void update(final AnActionEvent event) {
        Presentation presentation = event.getPresentation();
        Project project = event.getData(CommonDataKeys.PROJECT);

        try {
            if (project == null || project.isDisposed() || !project.isInitialized()) {
                presentation.setText("");
                presentation.setIcon(null);
                presentation.setEnabled(false);
            } else {
//                String name = settings.getName();
                presentation.setEnabled(true);
            }
        } catch (IndexNotReadyException e1) {
            presentation.setEnabled(false);
        }
    }

    @NotNull
    @Override
    protected DefaultActionGroup createPopupActionGroup(JComponent button) {
        DefaultActionGroup phaseGroup = new DefaultActionGroup();
        final Project project = CommonDataKeys.PROJECT.getData(DataManager.getInstance().getDataContext(button));

        if (project != null) {
            final RunManagerEx runManager = RunManagerEx.getInstanceEx(project);

        }

        return phaseGroup;
    }

    private void createEmptyList(DefaultActionGroup group) {
        AnAction action = new AnAction("No  recently viewed issues found") {

            public void actionPerformed(AnActionEvent anActionEvent) {

            }
        };
        action.setDefaultIcon(true);
        group.add(action);
    }
}
