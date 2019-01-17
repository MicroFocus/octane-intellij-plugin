//package com.hpe.adm.octane.ideplugins.intellij.actions;
//
//import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
//import com.hpe.adm.octane.ideplugins.intellij.gitcommit.CommitMessageUtils;
//import com.hpe.adm.octane.ideplugins.intellij.settings.IdePluginPersistentState;
//import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
//import com.hpe.adm.octane.ideplugins.intellij.ui.ToolbarActiveItem;
//import com.hpe.adm.octane.ideplugins.services.util.PartialEntity;
//import com.intellij.openapi.actionSystem.AnAction;
//import com.intellij.openapi.actionSystem.AnActionEvent;
//import com.intellij.openapi.actionSystem.CommonDataKeys;
//import com.intellij.openapi.project.Project;
//import com.intellij.openapi.util.IconLoader;
//
//public class StopActiveItemAction extends AnAction {
//
//        PartialEntity partialEntity;
//        CommitMessageUtils commitMessageUtils;
//
//        public StopActiveItemAction(PartialEntity partialEntity) {
//            super("Stop work on the active item", "Stops work on active item", IconLoader.findIcon(Constants.IMG_STOP_TIMER));
//            this.partialEntity = partialEntity;
//            PluginModule pluginModule = PluginModule.getPluginModuleForProject(project);
//            commitMessageUtils = pluginModule.getInstance(CommitMessageUtils.class);
//        }
//
//        public void setPartialEntity(PartialEntity partialEntity) {
//            this.partialEntity = partialEntity;
//        }
//
//        @Override
//        public void update(AnActionEvent e) {
//            Project eventProject = e.getDataContext().getData(CommonDataKeys.PROJECT);
//
//            boolean isVisible = partialEntity != null;
//
//            //Compare update actions source to the DI project
//            //This is to not show the active item of another project in the same IntelliJ toolbar
//            if (eventProject != null && !eventProject.equals(ToolbarActiveItem.this.project)) {
//                isVisible = false;
//            }
//
//            //Update visibility
//            e.getPresentation().setVisible(isVisible);
//        }
//
//
//        @Override
//        public void actionPerformed(AnActionEvent e) {
//            persistentState.clearState(IdePluginPersistentState.Key.ACTIVE_WORK_ITEM);
//        }
//    }