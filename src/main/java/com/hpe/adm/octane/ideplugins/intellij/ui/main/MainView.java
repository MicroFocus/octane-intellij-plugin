package com.hpe.adm.octane.ideplugins.intellij.ui.main;

import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;

import javax.swing.*;

public class MainView implements View {

    private final JPanel rootPanel = new JPanel();

    public MainView(){
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

}
