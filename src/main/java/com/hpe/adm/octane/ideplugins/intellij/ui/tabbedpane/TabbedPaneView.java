package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.intellij.icons.AllIcons;
import com.intellij.ide.ui.UISettings;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.ActionCallback;
import com.intellij.openapi.wm.IdeFocusManager;
import com.intellij.ui.tabs.TabInfo;
import com.intellij.ui.tabs.TabsListener;
import com.intellij.ui.tabs.TabsUtil;
import com.intellij.ui.tabs.UiDecorator;
import com.intellij.ui.tabs.impl.JBEditorTabs;
import com.intellij.ui.tabs.impl.TabLabel;
import com.intellij.util.BitUtil;
import com.intellij.util.ui.TimedDeadzone;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;

public class TabbedPaneView implements View {

    private static final String TABBED_PANE_PLACE = "OctaneTabbedPane";

    private final JPanel rootPanel;
    private CustomJBRunnerTabs editorTabs;

    @Inject
    public TabbedPaneView(Project project){
        rootPanel = new JPanel();
        rootPanel.setLayout(new BorderLayout(0, 0));

        //Init tabbed pane
        //DataContext dataContext = DataManager.getInstance().getDataContext();
        //Project project = DataKeys.PROJECT.getData(dataContext);

        editorTabs = new CustomJBRunnerTabs(project, ActionManager.getInstance(), IdeFocusManager.getGlobalInstance(), project);

        //Edit presentation
        editorTabs
                .setTabDraggingEnabled(true)
                .setUiDecorator(
                () -> new UiDecorator.UiDecoration(null, new Insets(TabsUtil.TAB_VERTICAL_PADDING, 8, TabsUtil.TAB_VERTICAL_PADDING, 8)))
                .setTabLabelActionsMouseDeadzone(TimedDeadzone.NULL)
                .setGhostsAlwaysVisible(true)
                .setTabLabelActionsAutoHide(false);

        setTabsContextMenu(editorTabs);

        //Close tab with middle click
        editorTabs.addTabMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if(SwingUtilities.isMiddleMouseButton(e) && e.getSource() instanceof TabLabel){
                    TabLabel tabLabel = (TabLabel) e.getSource();
                    if(isClosable(tabLabel.getInfo())){
                        editorTabs.removeTab(tabLabel.getInfo());
                    }
                }
            }
        });

        rootPanel.add(editorTabs.getComponent(), BorderLayout.CENTER);
    }

    private void setTabsContextMenu(JBEditorTabs editorTabs){
        DefaultActionGroup contextMenuActionGroup = new DefaultActionGroup();
        contextMenuActionGroup.addAction(new AnAction() {
            @Override
            public void update(final AnActionEvent e) {
                e.getPresentation().setText("Close");
            }
            @Override
            public void actionPerformed(AnActionEvent e) {
                if(isClosable(editorTabs.getTargetInfo())){
                    editorTabs.removeTab(editorTabs.getTargetInfo());
                }
            }
        });
        contextMenuActionGroup.addAction(new AnAction() {
            @Override
            public void update(final AnActionEvent e) {
                e.getPresentation().setText("Close Others");
            }

            @Override
            public void actionPerformed(AnActionEvent e) {
                closeAllExcept(editorTabs.getTargetInfo());
            }
        });
        contextMenuActionGroup.addAction(new AnAction() {
            @Override
            public void update(final AnActionEvent e) {
                e.getPresentation().setText("Close All");
            }

            @Override
            public void actionPerformed(AnActionEvent e) {
                closeAll();
            }
        });
        editorTabs.setPopupGroup(contextMenuActionGroup, TABBED_PANE_PLACE, true);
    }

    @Override
    public JComponent getComponent() {
        return editorTabs.getComponent();
    }


    public TabInfo addTab(String title, String tooltip, Icon icon, JComponent component) {
        return addTab(title, tooltip, icon, component, true);
    }

    public TabInfo addTab(String title, String tooltip, Icon icon, JComponent component, boolean isClosable) {
        TabInfo tabInfo = new TabInfo(component);

        if(icon != null) {
            tabInfo.setIcon(icon);
        }
        if(tooltip != null){
            tabInfo.setTooltipText(tooltip);
        }

        tabInfo.setText(title);

        if(isClosable) {
            addCloseActionToTab(tabInfo);
        }

        editorTabs.addTab(tabInfo);
        return tabInfo;
    }

    @NotNull
    public ActionCallback removeTab(TabInfo info) {
        return editorTabs.removeTab(info);
    }

    public int getIndexOf(TabInfo tabInfo) {
        return editorTabs.getIndexOf(tabInfo);
    }

    public List<TabInfo> getTabInfos(){
        return editorTabs.getTabs();
    }

    private void addCloseActionToTab(TabInfo tabInfo){
        DefaultActionGroup defaultActionGroup = new DefaultActionGroup();
        defaultActionGroup.addAction(new CloseTab(tabInfo));
        tabInfo.setTabLabelActions(defaultActionGroup, TABBED_PANE_PLACE);
    }


    private void closeAll(){
        closeAllExcept(null);
    }

    private void closeAllExcept(TabInfo exceptTabInfo){
        editorTabs.getTabs().forEach(tabInfo -> {
           if(isClosable(tabInfo) && !tabInfo.equals(exceptTabInfo)){
               editorTabs.removeTab(tabInfo);
           }
        });
    }

    private boolean isClosable(TabInfo tabInfo){
        return (tabInfo.getTabLabelActions()!=null && hasActionType(tabInfo.getTabLabelActions(), CloseTab.class));
    }

    private boolean hasActionType(ActionGroup actionGroup, Class<? extends AnAction> actionType){
        for(AnAction anAction : actionGroup.getChildren(null)){
            if(anAction.getClass().equals(actionType)){
                return true;
            }
        }
        return false;
    }

    public boolean hasTabWithTabInfo(TabInfo tabInfo){
        return editorTabs.getTabs().contains(tabInfo);
    }

    public void selectTabWithTabInfo(TabInfo tabInfo, boolean requestFocus){
        editorTabs.select(tabInfo, requestFocus);
    }

    public void setSearchRequestHandler(CustomJBRunnerTabs.SearchRequestHandler searchRequestHandler) {
        editorTabs.setSearchRequestHandler(searchRequestHandler);
    }

    public void addTabsListener(@NotNull TabsListener listener) {
        editorTabs.addListener(listener);
    }

    private class CloseTab extends AnAction implements DumbAware {

        private final TabInfo myTabInfo;

        CloseTab(TabInfo info) {
            myTabInfo = info;
        }

        @Override
        public void update(final AnActionEvent e) {
            e.getPresentation().setIcon(AllIcons.Actions.Close);
            e.getPresentation().setHoveredIcon(AllIcons.Actions.CloseHovered);
            e.getPresentation().setVisible(UISettings.getInstance().SHOW_CLOSE_BUTTON);
            e.getPresentation().setText("Close. Alt-click to close others.");
        }

        @Override
        public void actionPerformed(final AnActionEvent e) {
            if (BitUtil.isSet(e.getModifiers(), InputEvent.ALT_MASK)) {
                closeAllExcept(myTabInfo);
            } else {
                if (isClosable(myTabInfo)) {
                    editorTabs.removeTab(myTabInfo);
                }
            }
        }
    }

}
