package com.hpe.adm.octane.ideplugins.intellij.ui.views;


import com.hpe.adm.octane.ideplugins.intellij.ui.HasComponent;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.treeStructure.Tree;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

/**
 * This control what tabs will be opened
 * //TODO: actually implement the thing, this is just a skeleton
 */
public class FilteringView implements HasComponent {

    JPanel rootPanel = new JPanel();

    public FilteringView(){
        rootPanel = new JPanel();
        rootPanel.setLayout(new BorderLayout(0, 0));

        JPanel panel = new JPanel();
        rootPanel.add(panel, BorderLayout.CENTER);
        panel.setLayout(new BorderLayout(0, 0));

        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);
        panel.add(toolBar, BorderLayout.NORTH);

        JButton btnEditFilters = new JButton("+");
        btnEditFilters.setOpaque(true);
        btnEditFilters.setContentAreaFilled(false);
        btnEditFilters.setBorderPainted(false);
        toolBar.add(btnEditFilters);

        JPanel panelActiveItem = new JPanel();
        FlowLayout flowLayout = (FlowLayout) panelActiveItem.getLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        panel.add(panelActiveItem, BorderLayout.SOUTH);

        JLabel lblActiveWorkItem = new JLabel("Active work item:");
        panelActiveItem.add(lblActiveWorkItem);

        JXHyperlink hyperlinkCurrentWorkItem = new JXHyperlink();
        hyperlinkCurrentWorkItem.setAction(new AbstractAction("21312 - Implement IntelliJ Plugin") {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    Desktop.getDesktop().browse(new URI("https://goo.gl/Z7Ikmw"));
                } catch (IOException | URISyntaxException e1) {
                    e1.printStackTrace();
                }
            }
        });

        panelActiveItem.add(hyperlinkCurrentWorkItem);


        JBScrollPane scrollPane = new JBScrollPane();
        JPanel panelScrollPane = new JPanel(new BorderLayout(0,0));
        panelScrollPane.setBorder(new EmptyBorder(0,5,0,5));
        panelScrollPane.add(scrollPane, BorderLayout.CENTER);
        panel.add(panelScrollPane, BorderLayout.CENTER);

        Tree tree = new Tree();
        scrollPane.setViewportView(tree);
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }
}
