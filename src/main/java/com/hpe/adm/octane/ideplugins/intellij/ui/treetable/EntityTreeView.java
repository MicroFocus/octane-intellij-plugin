package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PacmanLoadingWidget;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.treeStructure.Tree;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class EntityTreeView implements View {

    private JPanel rootPanel;

    private Tree tree;
    private JBScrollPane scrollPane;

    private PacmanLoadingWidget loadingWidget = new PacmanLoadingWidget("Loading...");

    private JButton refreshButton;

    public EntityTreeView(){

        tree = new Tree();
        //tree.setUI(new FillingTreeUI());
        //tree.setLargeModel(true);
        //Init with an empty model
        tree.setModel(new EntityTreeModel());

        tree.setRootVisible(false);
        //tree.setCellRenderer(new EntityTreeCellRenderer());
        tree.addMouseListener(createTreeMouseListener());

        //Add it to the root panel
        rootPanel = new JPanel(new BorderLayout(0,0));
        scrollPane = new JBScrollPane();
        scrollPane.setBorder(BorderFactory.createEmptyBorder());

        rootPanel.add(scrollPane, BorderLayout.CENTER);
        scrollPane.setViewportView(tree);

        //Toolbar
        rootPanel.add(createToolbar(), BorderLayout.EAST);
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

    private JPanel createToolbar(){
        JPanel toolbar = new JPanel();
        toolbar.setBorder(new EmptyBorder(22, 0, 22, 0));
        toolbar.setLayout(new BoxLayout(toolbar, BoxLayout.Y_AXIS));

        refreshButton = new JButton("R");
        toolbar.add(refreshButton);

        JButton expandAllButton = new JButton("E");
        expandAllButton.addActionListener(event -> expandAllNodes(tree));
        toolbar.add(expandAllButton);

        JButton collapseAllButton = new JButton("C");
        collapseAllButton.addActionListener(event -> collapseAllNodes(tree));
        toolbar.add(collapseAllButton);

        return toolbar;
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
