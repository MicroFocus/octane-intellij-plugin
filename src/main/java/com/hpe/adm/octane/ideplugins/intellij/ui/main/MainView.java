package com.hpe.adm.octane.ideplugins.intellij.ui.main;

import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

public class MainView implements View<MainPresenter> {

    private final Border marginBorder = new EmptyBorder(5,5,5,5);
    private final JPanel rootPanel = new JPanel();
    private MainPresenter presenter;

    public MainView(MainPresenter presenter){
        this.presenter = presenter;
        rootPanel.setBorder(marginBorder);
        GridLayoutManager gridLayoutManager = new GridLayoutManager(1, 1);
        rootPanel.setLayout(gridLayoutManager);
    }

    public void setTabView(View view){
        GridConstraints gc = new GridConstraints();
        gc.setFill(GridConstraints.FILL_BOTH);
        rootPanel.add(view.getComponent(), gc);
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

    @Override
    public MainPresenter getPresenter() {
        return presenter;
    }

    @Override
    public void setPresenter(MainPresenter presenter) {
        this.presenter = presenter;
    }

}
