package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.tree;

import com.intellij.ui.treeStructure.Tree;
import com.intellij.util.ui.UIUtil;

import javax.swing.plaf.TreeUI;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;

/**
 * Will make the cell inside the row the width of the tree, had to @Override setUI
 */
public class FillingTree extends Tree {

    public FillingTree() {
    }

    public FillingTree(TreeNode root) {
        super(root);
    }

    public FillingTree(TreeModel treemodel) {
        super(treemodel);
    }


    @Override
    public void setUI(final TreeUI ui) {
        TreeUI actualUI = ui;
        if (!isCustomUI()) {
            if (!(ui instanceof FillingWideSelectionTreeUI) && isWideSelection() && !UIUtil.isUnderGTKLookAndFeel()) {
                actualUI = new FillingWideSelectionTreeUI(isWideSelection(), getWideSelectionBackgroundCondition());
            }
        }
        super.setUI(actualUI);
    }

}
