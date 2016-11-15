package com.hpe.adm.octane.ideplugins.intellij.ui.panels;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * Created by savencu on 11/9/2016.
 */
public class MainToolWindowlFactory implements ToolWindowFactory{
    private ToolWindow rootWindow;
    private JPanel rootView;
    private JTree myWorkTree;
    private JLabel labelOne;
    private JToolBar detailsViewToolBar;
    private JToolBar filtersViewToolBar;
    private JButton myWorkButton;
    private JTree filtersTree;
    private JLabel activeItemLabel;

    public MainToolWindowlFactory(){

    }
    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        rootWindow = toolWindow;
        this.createUIComponents();
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        Content content = contentFactory.createContent(rootView, "", false);
        rootWindow.getContentManager().addContent(content);
    }

    private void createUIComponents() {
        myWorkButton.setText("My Work");
        myWorkButton.setBorder(BorderFactory.createEmptyBorder());

        JComboBox jComboBox = new JComboBox(new String[]{"A", "B", "C"});
        filtersViewToolBar.add(jComboBox);
        activeItemLabel.setText("No Active Items");

        JButton button = new JButton("button");
        detailsViewToolBar.add(button);
        detailsViewToolBar.addSeparator();
        detailsViewToolBar.add(new JButton("button 2"));
        labelOne.setText("It just works");

    }

}
