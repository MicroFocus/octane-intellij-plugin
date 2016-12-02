package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.tree;

import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.AbstractLayoutCache;
import java.awt.*;

/**
 * Tree UI that sets the rendered cell's width to fill the view
 */
public class FillingTreeUI extends BasicTreeUI {

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