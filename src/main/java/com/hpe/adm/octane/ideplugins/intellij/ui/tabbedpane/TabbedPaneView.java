package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.ClosableJTabbedPane;

import javax.swing.*;
import java.awt.*;

public class TabbedPaneView implements View<TabbedPanePresenter> {

    private TabbedPanePresenter presenter;

    private final JPanel rootPanel;
    private ClosableJTabbedPane tabbedPane = new ClosableJTabbedPane();

    public TabbedPaneView(){
        rootPanel = new JPanel();
        rootPanel.setLayout(new BorderLayout(0, 0));
        rootPanel.add(tabbedPane, BorderLayout.CENTER);
    }

    public TabbedPaneView(TabbedPanePresenter presenter) {
        this.presenter = presenter;
        rootPanel = new JPanel();
        rootPanel.setLayout(new BorderLayout(0, 0));
        rootPanel.add(tabbedPane, BorderLayout.CENTER);
    }

    @Override
    public JComponent getComponent() {
        return tabbedPane;
    }

    public TabbedPanePresenter getPresenter() {
        return presenter;
    }

    @Override
    public void setPresenter(TabbedPanePresenter presenter) {
        this.presenter = presenter;
    }

    public void addTab(String title, Component component) {
        tabbedPane.addTab(title, component);
    }

}
