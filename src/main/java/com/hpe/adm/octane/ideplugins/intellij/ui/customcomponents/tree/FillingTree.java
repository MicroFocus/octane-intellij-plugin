package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.tree;

import javax.swing.*;

/**
 * Will make the cell inside the row the width of the tree, had to @Override setUI
 */
public class FillingTree extends JTree {

    FillingWideSelectionTreeUI ui = new FillingWideSelectionTreeUI();

    public FillingTree() {
        setUI(ui);
    }

}
