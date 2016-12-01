package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.tree;

import com.intellij.openapi.util.Condition;
import com.intellij.util.ui.tree.WideSelectionTreeUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.tree.AbstractLayoutCache;
import java.awt.*;

/**
 * Will make the cell inside the row the width of the tree
 */
public class FillingWideSelectionTreeUI extends WideSelectionTreeUI {

    public FillingWideSelectionTreeUI() {
    }

    public FillingWideSelectionTreeUI(boolean wideSelection, @NotNull Condition<Integer> wideSelectionCondition) {
        super(wideSelection, wideSelectionCondition);
    }

    @Override
    protected AbstractLayoutCache.NodeDimensions createNodeDimensions() {
        return new NodeDimensionsHandler() {
            @Override
            public Rectangle getNodeDimensions(
                    Object value, int row, int depth, boolean expanded,
                    Rectangle size) {
                Rectangle dimensions = super.getNodeDimensions(value, row,
                        depth, expanded, size);
                dimensions.width = tree.getWidth() - getRowX(row, depth);
                return dimensions;
            }
        };
    }

}
