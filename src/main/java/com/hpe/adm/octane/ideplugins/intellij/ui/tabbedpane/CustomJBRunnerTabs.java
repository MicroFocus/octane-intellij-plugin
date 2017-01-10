package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.intellij.execution.ui.layout.impl.JBRunnerTabs;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.IdeFocusManager;
import com.intellij.ui.DocumentAdapter;
import com.intellij.ui.SearchTextField;
import com.intellij.ui.tabs.TabInfo;
import com.intellij.ui.tabs.TabsListener;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

class CustomJBRunnerTabs extends JBRunnerTabs {

    Map<TabInfo, SearchTextField> searchFields = new HashMap<>();
    private String lastSearchText = "";

    public CustomJBRunnerTabs(@Nullable Project project, @NotNull ActionManager actionManager, IdeFocusManager focusManager, @NotNull Disposable parent) {
        super(project, actionManager, focusManager, parent);

        addListener(new TabsListener.Adapter() {
            @Override
            public void beforeSelectionChanged(TabInfo oldSelection, TabInfo newSelection) {
                //sync
                searchFields.get(newSelection).setText(lastSearchText);
            }

            @Override
            public void tabRemoved(TabInfo tabToRemove) {
                searchFields.remove(tabToRemove);
            }
        });

    }

    private SearchTextField createTextField(){
        SearchTextField searchTextField = new SearchTextField();
        searchTextField.addDocumentListener(new DocumentAdapter() {
            @Override
            protected void textChanged(DocumentEvent e) {
                lastSearchText = searchTextField.getText();
            }
        });
        return searchTextField;
    }

    @NotNull
    @Override
    public TabInfo addTab(TabInfo info) {
        JPanel searchPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        SearchTextField searchTextField = createTextField();
        searchPanel.add(searchTextField);
        searchFields.put(info, searchTextField);

        info.setSideComponent(searchPanel);
        return super.addTab(info);
    }


}
