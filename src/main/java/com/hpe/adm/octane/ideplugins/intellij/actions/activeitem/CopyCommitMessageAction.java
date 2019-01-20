package com.hpe.adm.octane.ideplugins.intellij.actions.activeitem;

import com.hpe.adm.octane.ideplugins.intellij.actions.OctanePluginAction;
import com.hpe.adm.octane.ideplugins.intellij.gitcommit.CommitMessageUtils;
import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.util.IconLoader;
import org.json.JSONObject;

public class CopyCommitMessageAction extends OctanePluginAction {

    public CopyCommitMessageAction() {
        super("Copy commit message", "Copies commit message for the current active item", IconLoader.findIcon(Constants.IMG_COPY_ICON));
    }

    @Override
    public void update(AnActionEvent e) {
        getPluginModule(e).ifPresent(pluginModule -> {
            JSONObject jsonObject = pluginModule.getInstance(IdePluginPersistentState.class).loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            e.getPresentation().setEnabled(jsonObject != null);
        });
    }


    @Override
    public void actionPerformed(AnActionEvent e) {
        getPluginModule(e).ifPresent(pluginModule -> {
            JSONObject jsonObject = pluginModule.getInstance(IdePluginPersistentState.class).loadState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
            if (jsonObject != null) {
                pluginModule.getInstance(CommitMessageUtils.class).asyncCopyCommitMessageToClipboard(PartialEntity.fromJsonObject(jsonObject));
            }
        });
    }
}