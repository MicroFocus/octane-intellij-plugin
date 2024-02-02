/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.CommonShortcuts;
import com.intellij.openapi.actionSystem.EmptyAction;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.actionSystem.ex.ActionUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ui.JBMenuItem;
import com.intellij.openapi.ui.popup.JBPopup;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBList;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.Consumer;
import com.intellij.util.ReflectionUtil;
import com.intellij.util.ui.JBInsets;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.TextUI;
import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * @author max
 */
public class CustomSearchTextField extends JPanel {

    private int myHistorySize = 5;
    private final MyModel myModel;
    private final TextFieldWithProcessing myTextField;

    private JBPopup myPopup;
    private JLabel myClearFieldLabel;
    private JLabel myToggleHistoryLabel;
    private JPopupMenu myNativeSearchPopup;
    private JMenuItem myNoItems;

    public CustomSearchTextField() {
        this(true);
    }

    public CustomSearchTextField(boolean historyEnabled) {
        super(new BorderLayout());

        myModel = new CustomSearchTextField.MyModel();

        myTextField = new TextFieldWithProcessing() {
            @Override
            public void processKeyEvent(final KeyEvent e) {
                if (preprocessEventForTextField(e)) return;
                super.processKeyEvent(e);
            }

            @Override
            public void setBackground(final Color bg) {
                super.setBackground(bg);
                if (myClearFieldLabel != null) {
                    myClearFieldLabel.setBackground(bg);
                }
                if (myToggleHistoryLabel != null) {
                    myToggleHistoryLabel.setBackground(bg);
                }
            }

            @Override
            public void setUI(TextUI ui) {
                if (SystemInfo.isMac) {
                    try {
                        Class<?> uiClass = UIUtil.isUnderIntelliJLaF() ? Class.forName("com.intellij.ide.ui.laf.intellij.MacIntelliJTextFieldUI")
                                : Class.forName("com.intellij.ide.ui.laf.darcula.ui.DarculaTextFieldUI");
                        Method method = ReflectionUtil.getMethod(uiClass, "createUI", JComponent.class);
                        if (method != null) {
                            super.setUI((TextUI)method.invoke(uiClass, this));
                            Class<?> borderClass = UIUtil.isUnderIntelliJLaF() ? Class.forName("com.intellij.ide.ui.laf.intellij.MacIntelliJTextBorder")
                                    : Class.forName("com.intellij.ide.ui.laf.darcula.ui.DarculaTextBorder");
                            setBorder((Border) ReflectionUtil.newInstance(borderClass));
                            setOpaque(false);
                        }
                        return;
                    }
                    catch (Exception ignored) {
                    }
                }
                super.setUI(ui);
            }
        };
        myTextField.setColumns(15);
        myTextField.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                onFocusLost();
                super.focusLost(e);
            }

            @Override
            public void focusGained(FocusEvent e) {
                onFocusGained();
                super.focusGained(e);
            }
        });
        add(myTextField, BorderLayout.CENTER);
        myTextField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_DOWN) {
                    if (isSearchControlUISupported() && myNativeSearchPopup != null) {
                        myNativeSearchPopup.show(myTextField, 5, myTextField.getHeight());
                    } else if (myPopup == null || !myPopup.isVisible()) {
                        showPopup();
                    }
                }
            }
        });

        myToggleHistoryLabel = new JLabel(new ImageIcon(Objects.requireNonNull(CustomSearchTextField.class.getResource(Constants.IMG_SEARCH_ICON))));
        myToggleHistoryLabel.setBorder(BorderFactory.createEmptyBorder(0, 3, 0, 0));
        myToggleHistoryLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        myToggleHistoryLabel.setOpaque(true);
        myToggleHistoryLabel.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                togglePopup();
            }
        });
        if (historyEnabled) {
            add(myToggleHistoryLabel, BorderLayout.WEST);
        }

        myClearFieldLabel = new JLabel(!JBColor.isBright() ? AllIcons.Actions.CloseHovered : AllIcons.Actions.Close);
        myClearFieldLabel.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 3));
        myClearFieldLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        myClearFieldLabel.setOpaque(true);
        add(myClearFieldLabel, BorderLayout.EAST);
        myClearFieldLabel.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                myTextField.setText("");
                onFieldCleared();
            }
        });

        final Border originalBorder;
        if (SystemInfo.isMac) {
            originalBorder = BorderFactory.createLoweredBevelBorder();
        }
        else {
            originalBorder = myTextField.getBorder();
        }

        myToggleHistoryLabel.setBackground(myTextField.getBackground());
        myClearFieldLabel.setBackground(myTextField.getBackground());

        setBorder(new CompoundBorder(BorderFactory.createEmptyBorder(2, 0, 0, 15), originalBorder));

        myTextField.setOpaque(true);
        myTextField.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));

        if (ApplicationManager.getApplication() != null) { //tests
            final ActionManager actionManager = ActionManager.getInstance();
            if (actionManager != null) {
                ActionUtil.wrap(IdeActions.ACTION_CLEAR_TEXT).registerCustomShortcutSet(CommonShortcuts.ESCAPE, this);
            }
        }
    }

    protected void onFieldCleared() {
    }

    protected void onFocusLost() {
    }

    protected void onFocusGained() {
    }

    private void updateMenu() {
        if (myNativeSearchPopup != null) {
            myNativeSearchPopup.removeAll();
            final int itemsCount = myModel.getSize();
            if (itemsCount == 0) {
                myNativeSearchPopup.add(myNoItems);
            }
            else {
                for (int i = 0; i < itemsCount; i++) {
                    final String item = myModel.getElementAt(i);
                    addMenuItem(item);
                }
            }
        }
    }

    protected boolean isSearchControlUISupported() {
        return (SystemInfo.isMac && UIUtil.isUnderAquaBasedLookAndFeel()) || !JBColor.isBright() || UIUtil.isUnderIntelliJLaF();
    }

    protected boolean hasIconsOutsideOfTextField() {
        return false;
    }

    public void addDocumentListener(DocumentListener listener) {
        getTextEditor().getDocument().addDocumentListener(listener);
    }

    public void removeDocumentListener(DocumentListener listener) {
        getTextEditor().getDocument().removeDocumentListener(listener);
    }

    public void addKeyboardListener(final KeyListener listener) {
        getTextEditor().addKeyListener(listener);
    }

    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        if (myToggleHistoryLabel != null) {
            final Color bg = enabled ? UIUtil.getTextFieldBackground() : UIUtil.getPanelBackground();
            myToggleHistoryLabel.setBackground(bg);
            myClearFieldLabel.setBackground(bg);
        }
    }

    public void setHistorySize(int historySize) {
        if (historySize <= 0) throw new IllegalArgumentException("history size must be a positive number");
        myHistorySize = historySize;
    }

    public void setHistory(List<String> aHistory) {
        myModel.setItems(aHistory);
    }

    public List<String> getHistory() {
        final int itemsCount = myModel.getSize();
        final List<String> history = new ArrayList<String>(itemsCount);
        for (int i = 0; i < itemsCount; i++) {
            history.add(myModel.getElementAt(i));
        }
        return history;
    }

    public void setText(String aText) {
        getTextEditor().setText(aText);
    }

    public String getText() {
        return getTextEditor().getText();
    }

    public void removeNotify() {
        super.removeNotify();
        hidePopup();
    }

    public void addCurrentTextToHistory() {
        if ((myNativeSearchPopup != null && myNativeSearchPopup.isVisible()) || (myPopup != null && myPopup.isVisible())) {
            return;
        }
        final String item = getText();
        myModel.addElement(item);
    }

    private void addMenuItem(final String item) {
        if (myNativeSearchPopup != null) {
            myNativeSearchPopup.remove(myNoItems);
            final JMenuItem menuItem = new JBMenuItem(item);
            myNativeSearchPopup.add(menuItem);
            menuItem.addActionListener(new ActionListener() {
                public void actionPerformed(final ActionEvent e) {
                    myTextField.setText(item);
                    //addCurrentTextToHistory();
                    if(historyItemClickedHandler!=null){
                        historyItemClickedHandler.run();
                    }
                }
            });
        }
    }

    public void selectText() {
        getTextEditor().selectAll();
    }

    public JBTextField getTextEditor() {
        return myTextField;
    }

    public boolean requestFocusInWindow() {
        return myTextField.requestFocusInWindow();
    }

    public void requestFocus() {
        getTextEditor().requestFocus();
    }

    public class MyModel extends AbstractListModel {
        private List<String> myFullList = new ArrayList<String>();

        private String mySelectedItem;

        public String getElementAt(int index) {
            return myFullList.get(index);
        }

        public int getSize() {
            return Math.min(myHistorySize, myFullList.size());
        }

        public void addElement(String item) {
            final String newItem = item.trim();
            if (newItem.isEmpty()) {
                return;
            }

            final int length = myFullList.size();
            int index = -1;
            for (int i = 0; i < length; i++) {
                if (StringUtil.equalsIgnoreCase(myFullList.get(i), newItem)) {
                    index = i;
                    break;
                }
            }
            if (index == 0) {
                // item is already at the top of the list
                return;
            }
            else if (index > 0) {
                // move item to top of the list
                myFullList.remove(index);
            }
            else if (myFullList.size() >= myHistorySize && myFullList.size() > 0) {
                // trim list
                myFullList.remove(myFullList.size() - 1);
            }
            insertElementAt(newItem, 0);
        }

        public void insertElementAt(String item, int index) {
            myFullList.add(index, item);
            fireContentsChanged();
        }

        public String getSelectedItem() {
            return mySelectedItem;
        }

        public void setSelectedItem(String anItem) {
            mySelectedItem = anItem;
        }

        public void fireContentsChanged() {
            fireContentsChanged(this, -1, -1);
            updateMenu();
        }

        public void setItems(List<String> aList) {
            myFullList = new ArrayList<String>(aList);
            fireContentsChanged();
        }
    }

    private void hidePopup() {
        if (myPopup != null) {
            myPopup.cancel();
            myPopup = null;
        }
    }

    @Override
    public Dimension getPreferredSize() {
        Dimension size = super.getPreferredSize();
        size.width += 50;
        Border border = super.getBorder();
        if (border != null && UIUtil.isUnderAquaBasedLookAndFeel()) {
            JBInsets.addTo(size, border.getBorderInsets(this));
        }
        return size;
    }

    protected void showPopup() {
        if (myPopup == null || !myPopup.isVisible()) {
            final JList list = new JBList(myModel);
            final List arrayList = myModel.myFullList;
            final Consumer chooseRunnable = createItemChosenCallback(list);
            myPopup = JBPopupFactory.getInstance().createPopupChooserBuilder(arrayList)
                    .setMovable(false)
                    .setRequestFocus(true)
                    .setItemChosenCallback(chooseRunnable).createPopup();
            if (isShowing()) {
                myPopup.showUnderneathOf(getPopupLocationComponent());
            }
        }
    }

    protected Component getPopupLocationComponent() {
        return this;
    }

    private void togglePopup() {
        if (myPopup == null) {
            showPopup();
        }
        else {
            hidePopup();
        }
    }

    public void setSelectedItem(final String s) {
        getTextEditor().setText(s);
    }

    public int getSelectedIndex() {
        return myModel.myFullList.indexOf(getText());
    }

    protected static class TextFieldWithProcessing extends JBTextField {
        public void processKeyEvent(KeyEvent e) {
            super.processKeyEvent(e);
        }
    }

    public final void keyEventToTextField(KeyEvent e) {
        myTextField.processKeyEvent(e);
    }

    protected boolean preprocessEventForTextField(KeyEvent e) {
        return false;
    }

    public void setSearchIcon(final Icon icon) {
        if (! isSearchControlUISupported()) {
            myToggleHistoryLabel.setIcon(icon);
        }
    }

    protected Consumer createItemChosenCallback(final JList list) {
        return (value) -> {
            getTextEditor().setText(value.toString() != null ? value.toString() : "");
            //addCurrentTextToHistory();
            if(historyItemClickedHandler!=null){
                historyItemClickedHandler.run();
            }
        };
    }

    private Runnable historyItemClickedHandler;

    public void setHistoryItemClickedHandler(Runnable runnable){
        historyItemClickedHandler = runnable;
    }

}
