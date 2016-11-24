package com.hpe.adm.octane.ideplugins.intellij.gitcommit;

import com.intellij.openapi.vcs.CheckinProjectPanel;
import com.intellij.openapi.vcs.checkin.CheckinHandler;

/**
 * Created by dulaut on 11/22/2016.
 */
public class OctaneCheckinHandler extends CheckinHandler {

    private CheckinProjectPanel panel;

    public OctaneCheckinHandler(CheckinProjectPanel panel) {
        this.panel = panel;
        System.out.println("init OctaneCheckinHandler");
        panel.setCommitMessage("number of files to commit changes on = " + panel.getFiles().size());
    }
}
