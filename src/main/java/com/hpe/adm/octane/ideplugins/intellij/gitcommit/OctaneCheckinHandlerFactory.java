package com.hpe.adm.octane.ideplugins.intellij.gitcommit;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.services.nonentity.CommitMessageService;
import com.intellij.openapi.vcs.CheckinProjectPanel;
import com.intellij.openapi.vcs.changes.CommitContext;
import com.intellij.openapi.vcs.checkin.CheckinHandler;
import com.intellij.openapi.vcs.checkin.CheckinHandlerFactory;
import com.intellij.openapi.vcs.ui.RefreshableOnComponent;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class OctaneCheckinHandlerFactory extends CheckinHandlerFactory {

    @NotNull
    @Override
    public CheckinHandler createHandler(@NotNull CheckinProjectPanel panel, @NotNull CommitContext commitContext) {
        if (!PluginModule.hasProject(panel.getProject())) {
            return CheckinHandler.DUMMY;
        }

        CheckinHandler checkinHandler = new OctaneCheckinHandler(
                PluginModule.getInstance(panel.getProject(), IdePluginPersistentState.class),
                PluginModule.getInstance(panel.getProject(), CommitMessageService.class),
                panel
        );
        return checkinHandler;
    }
}