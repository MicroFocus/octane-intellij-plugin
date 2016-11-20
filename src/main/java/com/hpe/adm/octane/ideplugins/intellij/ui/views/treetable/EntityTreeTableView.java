package com.hpe.adm.octane.ideplugins.intellij.ui.views.treetable;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.hpe.adm.octane.ideplugins.intellij.ui.components.PacmanLoadingWidget;
import com.hpe.adm.octane.ideplugins.services.TestService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.jdesktop.swingx.JXTreeTable;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collections;

public class EntityTreeTableView implements HasComponent {

    private JPanel rootPanel;

    private JXTreeTable entitiesTreeTable;
    private JScrollPane entitiesTreeTableScrollPane;

    private LoadTableActionListener loadTableActionListener = new LoadTableActionListener();

    private TestService testService = PluginModule.getInstance(TestService.class);

    public EntityTreeTableView(){

        entitiesTreeTable = new JXTreeTable(new EntityTreeTableModel(Collections.emptyList(), ""));
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

    private JPanel createToolbar(){
        JPanel toolbar = new JPanel();
        toolbar.setBorder(new EmptyBorder(22, 0, 22, 0));
        toolbar.setLayout(new BoxLayout(toolbar, BoxLayout.Y_AXIS));

        JButton refreshButton = new JButton("R");
        refreshButton.addActionListener(event -> loadTableActionListener.actionPerformed(event));
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

    private void setLoading(boolean isLoading){

        SwingUtilities.invokeLater(() -> {
            if(isLoading) {
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

    class LoadTableActionListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent e) {
            //Async get
            SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
                @Override
                protected Void doInBackground() throws Exception {
                    setLoading(true);
                    entitiesTreeTable.setTreeTableModel(new EntityTreeTableModel(testService.findEntities(Entity.WORK_ITEM), "subtype"));
                    return null;
                }

                @Override
                protected void done(){
                    setLoading(false);
                }
            };
            worker.execute();
        }
    }

    @Override
    public JComponent getComponent() {
        //lazy load, model empty at the start
        loadTableActionListener.actionPerformed(null);
        return rootPanel;
    }
}
