package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PacmanLoadingWidget;
import org.jdesktop.swingx.JXTreeTable;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;

public class EntityTreeTableView implements HasComponent {

    private JPanel rootPanel;

    private JXTreeTable entitiesTreeTable;
    private JScrollPane entitiesTreeTableScrollPane;

    private EntityTreeTablePresenter presenter;

    public EntityTreeTableView(EntityTreeTablePresenter presenter, EntityTreeTableModel entityTreeTableModel){
        this.presenter = presenter;

        entitiesTreeTable = new JXTreeTable(entityTreeTableModel);
        entitiesTreeTable.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        entitiesTreeTable.setRootVisible(false);
        entitiesTreeTable.setEditable(false);
        entitiesTreeTable.setClosedIcon(null);
        entitiesTreeTable.setOpenIcon(null);
        entitiesTreeTable.setLeafIcon(null);
        entitiesTreeTable.setRowHeight(30);
        entitiesTreeTable.setColumnSelectionAllowed(false);
        entitiesTreeTable.setRowSelectionAllowed(true);

        //Add it to the root panel
        rootPanel = new JPanel(new BorderLayout(0,0));
        entitiesTreeTableScrollPane = new JScrollPane();
        entitiesTreeTableScrollPane.setBorder(BorderFactory.createEmptyBorder());
        rootPanel.add(entitiesTreeTableScrollPane, BorderLayout.CENTER);
        entitiesTreeTableScrollPane.setViewportView(entitiesTreeTable);

        //Column header settings
        //((DefaultTableCellRenderer)entitiesTreeTable.getTableHeader().getDefaultRenderer()).setHorizontalAlignment(JLabel.LEFT);

        //Set the header width to match the row width
        entitiesTreeTable.getTableHeader().setPreferredSize(new Dimension(entitiesTreeTableScrollPane.getWidth(), 25));

        //Toolbar
        rootPanel.add(createToolbar(), BorderLayout.EAST);

    }

    public EntityTreeTablePresenter getPresenter() {
        return presenter;
    }
    public void setPresenter(EntityTreeTablePresenter presenter) {
        this.presenter = presenter;
    }

    private JPanel createToolbar(){
        JPanel toolbar = new JPanel();
        toolbar.setBorder(new EmptyBorder(22, 0, 22, 0));
        toolbar.setLayout(new BoxLayout(toolbar, BoxLayout.Y_AXIS));

        JButton refreshButton = new JButton("R");
        refreshButton.addActionListener(event -> {
            presenter.refresh();
        });
        toolbar.add(refreshButton);

        JButton expandAllButton = new JButton("E");
        expandAllButton.addActionListener(event -> {
            entitiesTreeTable.expandAll();
        });
        toolbar.add(expandAllButton);
        JButton collapseAllButton = new JButton("C");
        collapseAllButton.addActionListener(event -> {
            entitiesTreeTable.collapseAll();
        });
        toolbar.add(collapseAllButton);

        JButton groupingButton = new JButton("G");
        groupingButton.addActionListener(event -> {
            //Adjust grouping
        });
        toolbar.add(groupingButton);

        JButton columnsButton = new JButton("C");
        columnsButton.addActionListener(event -> {
            //Adjust columns
        });
        toolbar.add(columnsButton);

        return toolbar;
    }

    private PacmanLoadingWidget loadingWidget = new PacmanLoadingWidget("Loading...");

    public void setLoading(boolean isLoading) {

        SwingUtilities.invokeLater(() -> {
            if (isLoading) {
                rootPanel.remove(entitiesTreeTableScrollPane);
                rootPanel.add(loadingWidget, BorderLayout.CENTER);
            } else {
                rootPanel.remove(loadingWidget);
                rootPanel.add(entitiesTreeTableScrollPane);
            }
            rootPanel.revalidate();
            rootPanel.repaint();
        });

    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }
}
