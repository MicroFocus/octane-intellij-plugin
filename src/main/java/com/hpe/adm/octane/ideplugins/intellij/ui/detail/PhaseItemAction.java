package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;


public class PhaseItemAction extends AnAction {
    private static final int SUMMARY_LENGHT = 10;
    private String phaseName = " ";

    public PhaseItemAction(String phase) {
        this.phaseName = phase;

        if (phaseName.length() > SUMMARY_LENGHT) {
            phaseName = phaseName.substring(0, SUMMARY_LENGHT) + "...";
        }
        getTemplatePresentation().setDescription(phaseName);
    }

    @Override
    public void actionPerformed(AnActionEvent e) {

    }

    public String getPhaseName() {
        return phaseName;
    }

    public void setPhaseName(String phaseName) {
        this.phaseName = phaseName;
    }
}
