/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

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


