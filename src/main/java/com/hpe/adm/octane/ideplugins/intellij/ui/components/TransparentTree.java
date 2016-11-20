package com.hpe.adm.octane.ideplugins.intellij.ui.components;

import javax.swing.*;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.util.Objects;

public class TransparentTree extends JTree {

    private static final Color SELC = new Color(100, 100, 255, 100);
    @Override protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setPaint(SELC);
        for (int i: getSelectionRows()) {
            Rectangle r = getRowBounds(i);
            g2.fillRect(0, r.y, getWidth(), r.height);
        }
        super.paintComponent(g);
        if (hasFocus()) {
            TreePath path = getLeadSelectionPath();
            if (Objects.nonNull(path)) {
                Rectangle r = getRowBounds(getRowForPath(path));
                g2.setPaint(SELC.darker());
                g2.drawRect(0, r.y, getWidth() - 1, r.height - 1);
            }
        }
        g2.dispose();
    }
    @Override public void updateUI() {
        super.updateUI();

        UIManager.put("Tree.repaintWholeRow", Boolean.TRUE);

        Icon empty = new TreeIcon();
        UIManager.put("Tree.closedIcon", empty);
        UIManager.put("Tree.openIcon", empty);
        UIManager.put("Tree.collapsedIcon", empty);
        UIManager.put("Tree.expandedIcon", empty);
        UIManager.put("Tree.leafIcon", empty);

        setCellRenderer(new TransparentTreeCellRenderer());
        setOpaque(false);
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    }

    //Empty icon
    class TreeIcon implements Icon {

        public TreeIcon() {
        }

        public int getIconWidth() {
            return 0;
        }

        public int getIconHeight() {
            return 0;
        }

        public void paintIcon(Component c, Graphics g, int x, int y) {}
    }
}

class TransparentTreeCellRenderer extends DefaultTreeCellRenderer {
    private static final Color ALPHA_OF_ZERO = new Color(0x0, true);
    @Override public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        JComponent c = (JComponent) super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, false);
        c.setOpaque(false);
        return c;
    }
    @Override public Color getBackgroundNonSelectionColor() {
        return ALPHA_OF_ZERO;
    }
    @Override public Color getBackgroundSelectionColor() {
        return ALPHA_OF_ZERO;
    }
}

class TranslucentTreeCellRenderer extends TransparentTreeCellRenderer {
    private final Color backgroundSelectionColor = new Color(100, 100, 255, 100);
    @Override public Color getBackgroundSelectionColor() {
        return backgroundSelectionColor;
    }
}


