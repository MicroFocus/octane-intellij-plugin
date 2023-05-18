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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.Conditions;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.util.ui.MouseEventAdapter;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.TreeUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

class FillingTree extends JTree {

    private FillingWideSelectionTreeUI ui = new FillingWideSelectionTreeUI();

    public FillingTree() {
        setUI(ui);
    }

    /**
     * This is a custom component, it should always have the same type of UI
     * @param ui
     */
    @Override
    public void setUI(final TreeUI ui) {
        if(ui instanceof  FillingWideSelectionTreeUI) {
            super.setUI(ui);
        } else {
            //this is probably triggered by a LAF change
            //Makes sure all the colors are right
            this.ui = new FillingWideSelectionTreeUI();

            //Reset the same type of ui with the correct colors from LAF
            super.setUI(this.ui);
        }
    }

    @Override
    public TreePath getPathForLocation(int x, int y) {
        return  getClosestPathForLocation(x, y);
    }

    /**
     * Will make the cell inside the row the width of the tree, had to @Override setUI
     */
    private static class FillingWideSelectionTreeUI extends BasicTreeUI {
        public static final String TREE_TABLE_TREE_KEY = "TreeTableTree";

        @NonNls
        public static final String SOURCE_LIST_CLIENT_PROPERTY = "mac.ui.source.list";
        @NonNls public static final String STRIPED_CLIENT_PROPERTY = "mac.ui.striped";

        private static final Border LIST_BACKGROUND_PAINTER = UIManager.getBorder("List.sourceListBackgroundPainter");
        private static final Border LIST_SELECTION_BACKGROUND_PAINTER = UIManager.getBorder("List.sourceListSelectionBackgroundPainter");
        private static final Border LIST_FOCUSED_SELECTION_BACKGROUND_PAINTER = UIManager.getBorder("List.sourceListFocusedSelectionBackgroundPainter");

        @NotNull
        private final Condition<Integer> myWideSelectionCondition;
        private boolean myWideSelection;
        private boolean myOldRepaintAllRowValue;
        private boolean myForceDontPaintLines = false;
        private boolean mySkinny = false;

        @SuppressWarnings("unchecked")
        public FillingWideSelectionTreeUI() {
            this(true, Conditions.alwaysTrue());
        }

        /**
         * Creates new <code>WideSelectionTreeUI</code> object.
         *
         * @param wideSelection           flag that determines if wide selection should be used
         * @param wideSelectionCondition  strategy that determine if wide selection should be used for a target row (it's zero-based index
         *                                is given to the condition as an argument)
         */
        public FillingWideSelectionTreeUI(final boolean wideSelection, @NotNull Condition<Integer> wideSelectionCondition) {
            myWideSelection = wideSelection;
            myWideSelectionCondition = wideSelectionCondition;
        }

        @Override
        public int getRightChildIndent() {
            return isSkinny() ? 8 : super.getRightChildIndent();
        }

        public boolean isSkinny() {
            return mySkinny;
        }

        /**
         * Setting to <code>true</code> make tree to reduce row offset
         * @param skinny <code>true</code> to reduce row offset
         */
        public void setSkinny(boolean skinny) {
            mySkinny = skinny;
        }

        @Override
        protected MouseListener createMouseListener() {
            return new MouseEventAdapter<MouseListener>(super.createMouseListener()) {
                @Override
                public void mouseDragged(MouseEvent event) {
                    JTree tree = (JTree)event.getSource();
                    Object property = tree.getClientProperty("DnD Source"); // DnDManagerImpl.SOURCE_KEY
                    if (property == null) {
                        super.mouseDragged(event); // use Swing-based DnD only if custom DnD is not set
                    }
                }

                @Override
                protected MouseEvent convert(MouseEvent event) {
                    if (!event.isConsumed() && SwingUtilities.isLeftMouseButton(event)) {
                        int x = event.getX();
                        int y = event.getY();
                        JTree tree = (JTree)event.getSource();
                        if (tree.isEnabled()) {
                            TreePath path = getClosestPathForLocation(tree, x, y);
                            if (path != null && !isLocationInExpandControl(path, x, y)) {
                                Rectangle bounds = getPathBounds(tree, path);
                                if (bounds != null && bounds.y <= y && y <= (bounds.y + bounds.height)) {
                                    x = Math.max(bounds.x, Math.min(x, bounds.x + bounds.width - 1));
                                    if (x != event.getX()) {
                                        event = convert(event, tree, x, y);
                                    }
                                }
                            }
                        }
                    }
                    return event;
                }
            };
        }

        @Override
        protected void completeUIInstall() {
            super.completeUIInstall();

            myOldRepaintAllRowValue = UIManager.getBoolean("Tree.repaintWholeRow");
            UIManager.put("Tree.repaintWholeRow", true);

            tree.setShowsRootHandles(true);
        }

        @Override
        public void uninstallUI(JComponent c) {
            super.uninstallUI(c);

            UIManager.put("Tree.repaintWholeRow", myOldRepaintAllRowValue);
        }

        @Override
        protected void installKeyboardActions() {
            super.installKeyboardActions();

            if (Boolean.TRUE.equals(tree.getClientProperty("MacTreeUi.actionsInstalled"))) return;

            tree.putClientProperty("MacTreeUi.actionsInstalled", Boolean.TRUE);

            final InputMap inputMap = tree.getInputMap(JComponent.WHEN_FOCUSED);
            inputMap.put(KeyStroke.getKeyStroke("pressed LEFT"), "collapse_or_move_up");
            inputMap.put(KeyStroke.getKeyStroke("pressed RIGHT"), "expand");

            final ActionMap actionMap = tree.getActionMap();

            final Action expandAction = actionMap.get("expand");
            if (expandAction != null) {
                actionMap.put("expand", new TreeUIAction() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        final Object source = e.getSource();
                        if (source instanceof JTree) {
                            JTree tree = (JTree)source;
                            int selectionRow = tree.getLeadSelectionRow();
                            if (selectionRow != -1) {
                                TreePath selectionPath = tree.getPathForRow(selectionRow);
                                if (selectionPath != null) {
                                    boolean leaf = tree.getModel().isLeaf(selectionPath.getLastPathComponent());
                                    int toSelect = -1;
                                    int toScroll = -1;
                                    if (!leaf && tree.isExpanded(selectionRow)) {
                                        if (selectionRow + 1 < tree.getRowCount()) {
                                            toSelect = selectionRow + 1;
                                            toScroll = toSelect;
                                        }
                                    } else if (leaf) {
                                        toScroll = selectionRow;
                                    }

                                    if (toSelect != -1) {
                                        tree.setSelectionInterval(toSelect, toSelect);
                                    }

                                    if (toScroll != -1) {
                                        tree.scrollRowToVisible(toScroll);
                                    }

                                    if (toSelect != -1 || toScroll != -1) return;
                                }
                            }
                        }


                        expandAction.actionPerformed(e);
                    }
                });
            }

            actionMap.put("collapse_or_move_up", new TreeUIAction() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    final Object source = e.getSource();
                    if (source instanceof JTree) {
                        JTree tree = (JTree)source;
                        int selectionRow = tree.getLeadSelectionRow();
                        if (selectionRow == -1) return;

                        TreePath selectionPath = tree.getPathForRow(selectionRow);
                        if (selectionPath == null) return;

                        if (tree.getModel().isLeaf(selectionPath.getLastPathComponent()) || tree.isCollapsed(selectionRow)) {
                            final TreePath parentPath = tree.getPathForRow(selectionRow).getParentPath();
                            if (parentPath != null) {
                                if (parentPath.getParentPath() != null || tree.isRootVisible()) {
                                    final int parentRow = tree.getRowForPath(parentPath);
                                    tree.scrollRowToVisible(parentRow);
                                    tree.setSelectionInterval(parentRow, parentRow);
                                }
                            }
                        }
                        else {
                            tree.collapseRow(selectionRow);
                        }
                    }
                }
            });
        }

        public void setForceDontPaintLines() {
            myForceDontPaintLines = true;
        }

        private abstract static class TreeUIAction extends AbstractAction implements UIResource {
        }

        @Override
        protected int getRowX(int row, int depth) {
            if (isSkinny()) {
                int off = tree.isRootVisible() ? 8 : 0;
                return 8 * depth + 8 + off;
            } else {
                return super.getRowX(row, depth);
            }
        }

        @Override
        protected void paintHorizontalPartOfLeg(final Graphics g,
                                                final Rectangle clipBounds,
                                                final Insets insets,
                                                final Rectangle bounds,
                                                final TreePath path,
                                                final int row,
                                                final boolean isExpanded,
                                                final boolean hasBeenExpanded,
                                                final boolean isLeaf) {
            if (shouldPaintLines()) {
                super.paintHorizontalPartOfLeg(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
            }
        }

        private boolean shouldPaintLines() {
            return false;
        }

        @Override
        protected boolean isToggleSelectionEvent(MouseEvent e) {
            return SwingUtilities.isLeftMouseButton(e) && (SystemInfo.isMac ? e.isMetaDown() : e.isControlDown()) && !e.isPopupTrigger();
        }

        @Override
        protected void paintVerticalPartOfLeg(final Graphics g, final Rectangle clipBounds, final Insets insets, final TreePath path) {
            if (shouldPaintLines()) {
                super.paintVerticalPartOfLeg(g, clipBounds, insets, path);
            }
        }

        @Override
        protected void paintVerticalLine(Graphics g, JComponent c, int x, int top, int bottom) {
            if (shouldPaintLines()) {
                super.paintVerticalLine(g, c, x, top, bottom);
            }
        }

        @Override
        protected Color getHashColor() {
            return super.getHashColor();
        }

        @Override
        protected void paintRow(final Graphics g,
                                final Rectangle clipBounds,
                                final Insets insets,
                                final Rectangle bounds,
                                final TreePath path,
                                final int row,
                                final boolean isExpanded,
                                final boolean hasBeenExpanded,
                                final boolean isLeaf) {

            final int containerWidth = tree.getParent() instanceof JViewport ? tree.getParent().getWidth() : tree.getWidth();

            if(isLeaf) {
                bounds.x = 15;
            }
            bounds.width = containerWidth - bounds.x;

            final int xOffset = tree.getParent() instanceof JViewport ? ((JViewport)tree.getParent()).getViewPosition().x : 0;

            if (path != null && myWideSelection) {
                boolean selected = tree.isPathSelected(path);
                Graphics2D rowGraphics = (Graphics2D)g.create();
                rowGraphics.setClip(clipBounds);

                final Object sourceList = tree.getClientProperty(SOURCE_LIST_CLIENT_PROPERTY);
                Color background = tree.getBackground();

                if ((row % 2) == 0 && Boolean.TRUE.equals(tree.getClientProperty(STRIPED_CLIENT_PROPERTY))) {
                    background = UIUtil.getDecoratedRowColor();
                }

                if (sourceList != null && (Boolean)sourceList) {
                    if (selected) {
                        if (tree.hasFocus()) {
                            LIST_FOCUSED_SELECTION_BACKGROUND_PAINTER.paintBorder(tree, rowGraphics, xOffset, bounds.y, containerWidth, bounds.height);
                        }
                        else {
                            LIST_SELECTION_BACKGROUND_PAINTER.paintBorder(tree, rowGraphics, xOffset, bounds.y, containerWidth, bounds.height);
                        }
                    }
                    else if (myWideSelectionCondition.value(row)) {
                        rowGraphics.setColor(background);
                        rowGraphics.fillRect(xOffset, bounds.y, containerWidth, bounds.height);
                    }
                }
                else {
                    if (selected && (UIUtil.isUnderAquaBasedLookAndFeel() || UIUtil.isUnderDarcula() || UIUtil.isUnderIntelliJLaF())) {
                        Color bg = getSelectionBackground(tree, true);

                        if (myWideSelectionCondition.value(row)) {
                            rowGraphics.setColor(bg);
                            rowGraphics.fillRect(xOffset, bounds.y, containerWidth, bounds.height);
                        }
                    }
                }

                if (shouldPaintExpandControl(path, row, isExpanded, hasBeenExpanded, isLeaf)) {
                    paintExpandControl(rowGraphics, bounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
                }

                customPaintRow(rowGraphics, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
                rowGraphics.dispose();
            }
            else {
                customPaintRow(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
            }
        }

        /**
         * Paints the renderer part of a row. The receiver should
         * NOT modify <code>clipBounds</code>, or <code>insets</code>.
         */
        protected void customPaintRow(Graphics g, Rectangle clipBounds,
                                      Insets insets, Rectangle bounds, TreePath path,
                                      int row, boolean isExpanded,
                                      boolean hasBeenExpanded, boolean isLeaf) {
            // Don't paint the renderer if editing this row.
            if(editingComponent != null && editingRow == row)
                return;

            int leadIndex;

            if(tree.hasFocus()) {
                leadIndex = getLeadSelectionRow();
            }
            else
                leadIndex = -1;

            Component component;

            try{
                component = currentCellRenderer.getTreeCellRendererComponent
                        (tree, path.getLastPathComponent(),
                                tree.isRowSelected(row), isExpanded, isLeaf, row,
                                (leadIndex == row));
            } catch (Exception ex){
                component = new JLabel("N/A");
            }

            rendererPane.paintComponent(g, component, tree, bounds.x, bounds.y,
                    bounds.width, bounds.height, true);
        }



        @Override
        public void paint(Graphics g, JComponent c) {
            if (myWideSelection && !UIUtil.isUnderAquaBasedLookAndFeel() && !UIUtil.isUnderDarcula() && !UIUtil.isUnderIntelliJLaF()) {
                paintSelectedRows(g, ((JTree)c));
            }
            if (myWideSelection) {
                final int containerWidth = tree.getParent() instanceof JViewport ? tree.getParent().getWidth() : tree.getWidth();
                final int xOffset = tree.getParent() instanceof JViewport ? ((JViewport)tree.getParent()).getViewPosition().x : 0;
                final Rectangle bounds = g.getClipBounds();

                // draw background for the given clip bounds
                final Object sourceList = tree.getClientProperty(SOURCE_LIST_CLIENT_PROPERTY);
                if (sourceList != null && (Boolean)sourceList) {
                    Graphics2D backgroundGraphics = (Graphics2D)g.create();
                    backgroundGraphics.setClip(xOffset, bounds.y, containerWidth, bounds.height);
                    LIST_BACKGROUND_PAINTER.paintBorder(tree, backgroundGraphics, xOffset, bounds.y, containerWidth, bounds.height);
                    backgroundGraphics.dispose();
                }
            }

            super.paint(g, c);
        }

        protected void paintSelectedRows(Graphics g, JTree tr) {
            final Rectangle rect = tr.getVisibleRect();
            final int firstVisibleRow = tr.getClosestRowForLocation(rect.x, rect.y);
            final int lastVisibleRow = tr.getClosestRowForLocation(rect.x, rect.y + rect.height);

            for (int row = firstVisibleRow; row <= lastVisibleRow; row++) {
                if (tr.getSelectionModel().isRowSelected(row) && myWideSelectionCondition.value(row)) {
                    final Rectangle bounds = tr.getRowBounds(row);
                    Color color = getSelectionBackground(tr, false);
                    if (color != null) {
                        g.setColor(color);
                        g.fillRect(0, bounds.y, tr.getWidth(), bounds.height);
                    }
                }
            }
        }

        @Override
        protected CellRendererPane createCellRendererPane() {
            return new CellRendererPane() {
                @Override
                public void paintComponent(Graphics g, Component c, Container p, int x, int y, int w, int h, boolean shouldValidate) {
                    if (c instanceof JComponent && myWideSelection) {
                        if (c.isOpaque()) {
                            ((JComponent)c).setOpaque(false);
                        }
                    }

                    super.paintComponent(g, c, p, x, y, w, h, shouldValidate);
                }
            };
        }

        @Override
        protected void paintExpandControl(Graphics g,
                                          Rectangle clipBounds,
                                          Insets insets,
                                          Rectangle bounds,
                                          TreePath path,
                                          int row,
                                          boolean isExpanded,
                                          boolean hasBeenExpanded,
                                          boolean isLeaf) {
            boolean isPathSelected = tree.getSelectionModel().isPathSelected(path);
            if (!isLeaf(row)) {
                setExpandedIcon(UIUtil.getTreeNodeIcon(true, isPathSelected, tree.hasFocus()));
                setCollapsedIcon(UIUtil.getTreeNodeIcon(false, isPathSelected, tree.hasFocus()));
            }

            super.paintExpandControl(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
        }

        @Nullable
        private static Color getSelectionBackground(@NotNull JTree tree, boolean checkProperty) {
            Object property = tree.getClientProperty(TREE_TABLE_TREE_KEY);
            if (property instanceof JTable) {
                return ((JTable)property).getSelectionBackground();
            }
            boolean selection = tree.hasFocus();
            if (!selection && checkProperty) {
                selection = Boolean.TRUE.equals(property);
            }
            return UIUtil.getTreeSelectionBackground(selection);
        }
    }
}
