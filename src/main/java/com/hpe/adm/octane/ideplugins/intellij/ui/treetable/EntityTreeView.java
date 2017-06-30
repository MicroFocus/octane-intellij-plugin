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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.LoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.NoWorkPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.EntityContextMenuFactory;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.*;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import org.jdesktop.swingx.JXLabel;

import javax.inject.Provider;
import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import static javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER;

public class EntityTreeView implements View {

    public static class ExpandNodesAction extends AnAction {
        EntityTreeView treeView;
        public ExpandNodesAction(EntityTreeView treeView) {
            super("Expand all", "Expand all nodes of the tree", AllIcons.Actions.Expandall);
            this.treeView = treeView;
        }

        public void actionPerformed(AnActionEvent e) {
            treeView.expandAllNodes();
        }

    }

    public static class CollapseNodesAction extends AnAction {
        EntityTreeView treeView;
        public CollapseNodesAction(EntityTreeView treeView) {
            super("Collapse all", "Collapse all nodes of the tree", AllIcons.Actions.Collapseall);
            this.treeView = treeView;
        }
        public void actionPerformed(AnActionEvent e) {
            treeView.collapseAllNodes();
        }
    }

    public interface EntityDoubleClickHandler {
        void entityDoubleClicked(MouseEvent mouseEvent, Entity entityType, Long entityId, EntityModel model);
    }

    public interface TreeViewKeyHandler {
        void keyPressed(KeyEvent keyEvent, Entity selectedEntityType, Long selectedEntityId, EntityModel model);
    }

    private JPanel rootPanel;
    private FillingTree tree;
    private JBScrollPane scrollPane;
    private DefaultActionGroup toolBarActionGroup = new DefaultActionGroup();
    private LoadingWidget loadingWidget = new LoadingWidget();
    private EntityContextMenuFactory entityContextMenuFactory;

    @Inject
    public EntityTreeView(TreeCellRenderer entityTreeCellRenderer) {

        scrollPane = new JBScrollPane();
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.setHorizontalScrollBarPolicy(HORIZONTAL_SCROLLBAR_NEVER);

        tree = initTree(entityTreeCellRenderer);

        //Add it to the root panel
        rootPanel = new JPanel(new BorderLayout(0, 0));
        rootPanel.add(scrollPane, BorderLayout.CENTER);

        scrollPane.setViewportView(tree);

        //Toolbar
        rootPanel.add(createToolbar(), BorderLayout.EAST);
    }

    private FillingTree initTree(TreeCellRenderer entityTreeCellRenderer){
        FillingTree tree = new FillingTree();
        //Init with an empty model
        tree.setModel(new EntityTreeModel());

        tree.setRootVisible(false);
        tree.setCellRenderer(entityTreeCellRenderer);

        //Init context menu using the factory
        tree.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                TreePath path = tree.getPathForLocation(e.getX(), e.getY());

                if (SwingUtilities.isRightMouseButton(e) && path != null) {
                    Object obj = path.getLastPathComponent();
                    if(obj instanceof EntityModel && entityContextMenuFactory!=null){
                        JPopupMenu popupMenu = entityContextMenuFactory.createContextMenu((EntityModel) obj);
                        popupMenu.show(e.getComponent(), e.getX(), e.getY());
                    }
                }
            }
        });


        tree.setRowHeight(50);
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        return tree;
    }

    public void addEntityKeyHandler(TreeViewKeyHandler handler) {
        tree.addKeyListener(new KeyAdapter() {
            public void keyPressed(KeyEvent e) {
                if (tree.getSelectionCount() == 1) {
                    TreePath selPath = tree.getSelectionPath();
                    Object object = selPath.getLastPathComponent();
                    if (object instanceof EntityModel) {
                        try {
                            EntityModel entityModel = (EntityModel) object;
                            Entity entityType = Entity.getEntityType(entityModel);
                            Long entityId = Long.parseLong(entityModel.getValue("id").getValue().toString());
                            handler.keyPressed(e, entityType, entityId, entityModel);
                        } catch (Exception ex) {
                            //TODO: logger and error bubble
                        }
                    }
                }
            }
        });
    }

    public void addEntityMouseHandler(EntityDoubleClickHandler handler) {
        tree.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                    int selRow = tree.getRowForLocation(e.getX(), e.getY());
                    TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
                    if (selRow != -1) {
                        Object object = selPath.getLastPathComponent();
                        if (object instanceof EntityModel) {
                            try {
                                EntityModel entityModel = (EntityModel) object;
                                Entity entityType = Entity.getEntityType(entityModel);
                                Long entityId = Long.parseLong(entityModel.getValue("id").getValue().toString());
                                handler.entityDoubleClicked(e, entityType, entityId, entityModel);
                            } catch (Exception ex) {
                                //TODO: logger and error bubble
                            }
                        }
                    }
                }
            });
    }

    public void addActionToToolbar(AnAction action) {
        toolBarActionGroup.addAction(action);
    }

    public void addSeparatorToToolbar() {
        toolBarActionGroup.addSeparator();
    }

    private JComponent createToolbar() {
        ActionToolbar actionToolBar = ActionManager.getInstance().createActionToolbar("My Work actions", toolBarActionGroup, false);
        actionToolBar.getComponent().setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, JBColor.border()));
        return actionToolBar.getComponent();
    }

    public void expandAllNodes() {
        int j = tree.getRowCount();
        int i = 0;
        while (i < j) {
            tree.expandRow(i);
            i += 1;
            j = tree.getRowCount();
        }
    }

    public void collapseAllNodes() {
        int j = tree.getRowCount();
        int i = 0;
        while (i < j) {
            tree.collapseRow(i);
            i += 1;
            j = tree.getRowCount();
        }
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

    public void setLoading(boolean isLoading) {
        SwingUtilities.invokeLater(() -> {
            if (isLoading) {
                rootPanel.remove(scrollPane);
                rootPanel.add(loadingWidget, BorderLayout.CENTER);
            } else {
                rootPanel.remove(loadingWidget);
                rootPanel.add(scrollPane);
            }
            rootPanel.revalidate();
            rootPanel.repaint();
        });
    }

    public void setErrorMessage(String errorMessage) {
        JPanel errorPanel = new JPanel(new BorderLayout(0, 0));
        JXLabel errorLabel = new JXLabel("<html><center>" + errorMessage + "</center></html>");
        errorLabel.setVerticalAlignment(SwingConstants.CENTER);
        errorLabel.setHorizontalAlignment(SwingConstants.CENTER);
        errorPanel.add(errorLabel, BorderLayout.CENTER);
        errorLabel.setForeground(Color.RED);
        scrollPane.setViewportView(errorPanel);
    }

    public EntityTreeModel getTreeModel() {
        return (EntityTreeModel) tree.getModel();
    }

    public void setTreeModel(EntityTreeModel model) {
        tree.setModel(model);

        if (model.size() == 0) {
            scrollPane.setViewportView(componentWhenEmptyProvider.get());
        } else {
            scrollPane.setViewportView(tree);
        }
    }

    //Default value
    private Provider<JComponent> componentWhenEmptyProvider = () -> new NoWorkPanel();

    public void setComponentWhenEmpty(Provider<JComponent> componentWhenEmptyProvider){
        this.componentWhenEmptyProvider = componentWhenEmptyProvider;
    }

    public void setEntityContextMenuFactory(EntityContextMenuFactory entityContextMenuFactory){
        this.entityContextMenuFactory = entityContextMenuFactory;
    }

}