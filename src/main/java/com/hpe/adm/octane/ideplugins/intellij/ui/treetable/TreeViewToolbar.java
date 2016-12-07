package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.ui.SimpleToolWindowPanel;
import com.intellij.openapi.util.IconLoader;

import javax.swing.*;
import java.awt.*;


class TreeViewToolbar extends SimpleToolWindowPanel implements HasComponent {

    public TreeViewToolbar() {
        super(true, true);
        setToolbar(createToolbarPanel());
    }

    private JComponent createToolbarPanel() {
        final DefaultActionGroup group = new DefaultActionGroup();

        group.add(new RefreshAction());
        group.addSeparator();

        final ActionToolbar actionToolBar = ActionManager.getInstance().createActionToolbar("My Work Actions", group, false);

        final JPanel buttonsPanel = new JPanel(new BorderLayout());
        buttonsPanel.add(actionToolBar.getComponent(), BorderLayout.CENTER);

        return actionToolBar.getComponent();
    }

    public void addAction(AnAction action){

    }

    private final class RefreshAction extends AnAction {
        public RefreshAction() {
            super("Refresh OCTANE", "this will refresh the page", IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
        }

        public void actionPerformed(AnActionEvent e) {
            System.out.println("bla bla bla ");
        }
    }

    private final class ExpandNodesAction extends AnAction {
        public ExpandNodesAction() {
            super("Expand all", "Expand all nodes of the tree", AllIcons.Actions.Expandall);
        }

        public void actionPerformed(AnActionEvent e) {
            System.out.println("deleting");
        }

    }

    private final class CollapseNodesAction extends AnAction {
        public CollapseNodesAction() {
            super("Collapse all", "Collapse all nodes of the tree", AllIcons.Actions.Collapseall);
        }

        public void actionPerformed(AnActionEvent e) {
            System.out.println("running");

        }
    }
}
