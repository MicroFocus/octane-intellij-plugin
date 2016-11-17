package com.hpe.adm.octane.ideplugins.intellij.ui.custom;


import com.hpe.adm.octane.ideplugins.intellij.ui.custom.components.TransparentTree;
import org.apache.logging.log4j.core.appender.db.jpa.JpaAppender;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import java.awt.*;

/**
 * This control what tabs will be opened
 */
public class FilteringWidget implements HasComponent {

    JPanel panel = new JPanel();

    //For filter controls
    JPanel toolBar;

    //For actual filters
    JTree filterTree;

    //Current work item
    JButton currentWorkItem;


    public FilteringWidget(){
        toolBar = createToolbar();
        filterTree = createFilterTree();

        GridBagLayout gridBagLayout = new GridBagLayout();
        panel.setLayout(gridBagLayout);

        GridBagConstraints gbcToolbar = new GridBagConstraints();
        gbcToolbar.gridx = 0;
        gbcToolbar.gridy = 0;
        gbcToolbar.fill = GridBagConstraints.HORIZONTAL;
        panel.add(toolBar, gbcToolbar);

        addSeparator(panel, 1);

        GridBagConstraints gbcTree = new GridBagConstraints();
        gbcTree.gridx = 0;
        gbcTree.gridy = 2;
        gbcTree.fill = GridBagConstraints.BOTH;
        gbcTree.weightx = 1;
        gbcTree.weighty = 1;
        panel.add(filterTree, gbcTree);

        addSeparator(panel, 3);

        //Add a button that represent
        currentWorkItem = new JButton("1234 - Build IDE Plugins");
        GridBagConstraints gbcButton = new GridBagConstraints();
        gbcButton.gridx = 0;
        gbcButton.gridy = 4;
        gbcButton.fill = GridBagConstraints.HORIZONTAL;
        panel.add(currentWorkItem, gbcButton);
    }

    private void addSeparator(JPanel panel, int gridy){

        JSeparator sep = new JSeparator(JSeparator.HORIZONTAL);
        sep.setPreferredSize(new Dimension(-1 , 3));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = gridy;
        panel.add(sep, gbc);
    }

    private JPanel createToolbar(){
        JPanel toolBar = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEADING);
        flowLayout.setHgap(1);
        flowLayout.setVgap(4);
        toolBar.setLayout(flowLayout);
        toolBar.add(new JButton("+"));
        toolBar.add(new JButton("-"));
        return toolBar;
    }

    private JTree createFilterTree(){
        DefaultMutableTreeNode root = new DefaultMutableTreeNode();
        TreeModel model = new DefaultTreeModel(root);

        DefaultMutableTreeNode myWorkNode = new DefaultMutableTreeNode("My work");
        root.add(myWorkNode);

        DefaultMutableTreeNode filteringNode = new DefaultMutableTreeNode("Filtering");
        root.add(filteringNode);

        //TODO: these would be dynamic
        //Tree model would contain a filter object
        DefaultMutableTreeNode defectFilterNode = new DefaultMutableTreeNode("Defects");
        filteringNode.add(defectFilterNode);
        DefaultMutableTreeNode customFilterNode = new DefaultMutableTreeNode("My custom cool filter");
        filteringNode.add(customFilterNode);

        //Create the tree
        JTree filterTree = new TransparentTree();
        filterTree.setModel(model);
        filterTree.setRootVisible(false);

        //Expand all nodes at the start
        for (int i = 0; i < filterTree.getRowCount(); i++) {
            filterTree.expandRow(i);
        }

        filterTree.setCellRenderer ( new DefaultTreeCellRenderer()
        {
            private Border border = BorderFactory.createEmptyBorder ( 4, 4, 4, 4 );

            public Component getTreeCellRendererComponent ( JTree tree, Object value, boolean sel,
                                                            boolean expanded, boolean leaf, int row,
                                                            boolean hasFocus) {
                JLabel label = (JLabel) super.getTreeCellRendererComponent ( tree, value, sel, expanded, leaf, row, hasFocus );
                label.setBorder ( border );
                return label;
            }
        } );

        return filterTree;
    }

    @Override
    public JComponent getComponent() {
        return panel;
    }
}
