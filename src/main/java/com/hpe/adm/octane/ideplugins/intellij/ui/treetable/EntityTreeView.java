package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PacmanLoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.treeStructure.Tree;

import javax.swing.*;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class EntityTreeView implements View {

    public interface EntityDoubleClickHandler{
        void entityDoubleClicked(Entity entityType, Long entityId, EntityModel model);
    }

    private JPanel rootPanel;

    private Tree tree;
    private JBScrollPane scrollPane;

    private JButton refreshButton;

    public EntityTreeView(){

        tree = new Tree();
        //Init with an empty model
        tree.setModel(new EntityTreeModel());

        tree.setRootVisible(false);
        tree.setCellRenderer(new EntityTreeCellRenderer());
        //tree.addMouseListener(createTreeMouseListener());

        //Add it to the root panel
        rootPanel = new JPanel(new BorderLayout(0,0));
        scrollPane = new JBScrollPane();
        scrollPane.setBorder(BorderFactory.createEmptyBorder());

        rootPanel.add(scrollPane, BorderLayout.CENTER);
        scrollPane.setViewportView(tree);

        //Toolbar
        rootPanel.add(createToolbar(), BorderLayout.EAST);
    }

    public void addEntityDoubleClickHandler(EntityDoubleClickHandler handler){
        tree.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e)) {
                    int selRow = tree.getRowForLocation(e.getX(), e.getY());
                    TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
                    if (selRow != -1 && e.getClickCount() == 2) {
                        Object object = selPath.getLastPathComponent();
                        if(object instanceof EntityModel) {
                            try {
                                EntityModel entityModel = (EntityModel) object;
                                Entity entityType = Entity.getEntityType(entityModel);
                                Long entityId = Long.parseLong(entityModel.getValue("id").getValue().toString());
                                handler.entityDoubleClicked(entityType, entityId, entityModel);
                            } catch (Exception ex){
                                System.out.print(ex);
                            }
                        }
                    }
                }
            }
        });
    }

    private MouseListener createTreeMouseListener(){
        MouseListener ml = new MouseAdapter() {
            public void mousePressed(MouseEvent e) {

                if (SwingUtilities.isRightMouseButton(e)) {

                    int row = tree.getClosestRowForLocation(e.getX(), e.getY());
                    tree.setSelectionRow(row);
                    JPopupMenu popup = new JPopupMenu();
                    popup.add(new JMenuItem("Icecream!"));
                    popup.show(e.getComponent(), e.getX(), e.getY());

                } else if (SwingUtilities.isLeftMouseButton(e)) {
                    int selRow = tree.getRowForLocation(e.getX(), e.getY());
                    TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());

                    if (selRow != -1) {
                        if (e.getClickCount() == 1) {
                            System.out.println("SINGLE CLICKKKKK!");
                        } else if (e.getClickCount() == 2) {
                            System.out.println("DDDDDOUBLE CLICKKKKK!");
                        }
                    }
                }
            }
        };
        return ml;
    }

    private JComponent createToolbar(){

        JPanel toolbar = new JPanel();
        toolbar.setLayout(new BoxLayout(toolbar, BoxLayout.Y_AXIS));

        toolbar.setBorder(BorderFactory.createMatteBorder(0,1,0,0, JBColor.border()));

        refreshButton = createButton(IconLoader.findIcon(Constants.IMG_REFRESH_ICON));
        toolbar.add(refreshButton);

        JButton expandAllButton = createButton(AllIcons.Actions.Expandall);
        expandAllButton.addActionListener(event -> expandAllNodes(tree));
        toolbar.add(expandAllButton);

        JButton collapseAllButton = createButton(AllIcons.Actions.Collapseall);
        collapseAllButton.addActionListener(event -> collapseAllNodes(tree));
        toolbar.add(collapseAllButton);

        return toolbar;
    }

    private JButton createButton(Icon icon){
        JButton button = new JButton(icon);
        button.setBorderPainted(false);
        button.setContentAreaFilled(false);
        button.setOpaque(false);

        button.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                button.setBorderPainted(true);
                button.setContentAreaFilled(true);
                button.setOpaque(true);
            }

            public void mouseExited(java.awt.event.MouseEvent evt) {
                button.setBorderPainted(false);
                button.setContentAreaFilled(false);
                button.setOpaque(false);
            }
        });

        return button;
    }

    private void expandAllNodes(JTree tree) {
        int j = tree.getRowCount();
        int i = 0;
        while(i < j) {
            tree.expandRow(i);
            i += 1;
            j = tree.getRowCount();
        }
    }

    private void collapseAllNodes(JTree tree) {
        int j = tree.getRowCount();
        int i = 0;
        while (i < j) {
            tree.collapseRow(i);
            i += 1;
            j = tree.getRowCount();
        }
    }

    //TODO: create proper wrapped handler
    public void addRefreshButtonActionListener(ActionListener l) {
        refreshButton.addActionListener(l);
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

    private PacmanLoadingWidget loadingWidget = new PacmanLoadingWidget("Loading...");

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

    public void setTreeModel(TreeModel model){
        tree.setModel(model);
    }

    public EntityTreeModel getTreeModel(){
        return (EntityTreeModel) tree.getModel();
    }



}
