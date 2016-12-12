package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.tree;

import com.intellij.ui.treeStructure.Tree;

/**
 * Will make the cell inside the row the width of the tree, had to @Override setUI
 */
public class FillingTree extends Tree {

    FillingWideSelectionTreeUI ui = new FillingWideSelectionTreeUI();

    public FillingTree() {
        setUI(ui);
    }

    @Override
    protected boolean isCustomUI() {
        return true;
    }

}
