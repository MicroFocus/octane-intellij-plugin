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
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

class CustomJBRunnerTabs extends JBRunnerTabs {

    /**
     * Horrible workaround
     */
    Map<TabInfo, SearchTextField> searchFields = new HashMap<>();
    private String lastSearchText = "";

    public CustomJBRunnerTabs(@Nullable Project project, @NotNull ActionManager actionManager, IdeFocusManager focusManager, @NotNull Disposable parent) {
        super(project, actionManager, focusManager, parent);

        addListener(new TabsListener.Adapter() {
            @Override
            public void beforeSelectionChanged(TabInfo oldSelection, TabInfo newSelection) {
                //sync text and history
                if(searchFields.containsKey(oldSelection) && searchFields.containsKey(newSelection)){
                    searchFields.get(newSelection).setText(lastSearchText);
                    searchFields.get(newSelection).setHistory(searchFields.get(oldSelection).getHistory());
                }
            }

            @Override
            public void tabRemoved(TabInfo tabToRemove) {
                searchFields.remove(tabToRemove);
            }
        });

    }

    private SearchTextField createTextField(){
        SearchTextField currentSearchTextField = new SearchTextField();
        currentSearchTextField.addDocumentListener(new DocumentAdapter() {
            @Override
            protected void textChanged(DocumentEvent e) {
                lastSearchText = currentSearchTextField.getText();
            }
        });

        currentSearchTextField.addKeyboardListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if(e.getKeyCode() == KeyEvent.VK_ENTER &&
                        searchRequestHandler != null &&
                        StringUtils.isNotBlank(currentSearchTextField.getText())){
                    currentSearchTextField.addCurrentTextToHistory();
                    searchRequestHandler.searchedQuery(currentSearchTextField.getText());
                }
            }
        });

        return currentSearchTextField;
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

    public interface SearchRequestHandler {
        void searchedQuery(String query);
    }

    private SearchRequestHandler searchRequestHandler;

    public void setSearchRequestHandler(SearchRequestHandler searchRequestHandler){
        this.searchRequestHandler = searchRequestHandler;
    }

}