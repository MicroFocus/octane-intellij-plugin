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

package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.hpe.adm.octane.ideplugins.intellij.PluginModule;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.CustomSearchTextField;
import com.hpe.adm.octane.ideplugins.intellij.ui.searchresult.SearchHistoryManager;
import com.intellij.execution.ui.layout.impl.JBRunnerTabs;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.IdeFocusManager;
import com.intellij.ui.DocumentAdapter;
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
import java.util.List;
import java.util.Map;

class CustomJBRunnerTabs extends JBRunnerTabs {

    public interface SearchRequestHandler {
        void searchedQuery(String query);
    }

    private SearchRequestHandler searchRequestHandler;

    /**
     * Horrible workaround
     */
    Map<TabInfo, CustomSearchTextField> searchFields = new HashMap<>();

    private SearchHistoryManager searchManager;

    private String lastSearchText = "";

    public CustomJBRunnerTabs(@Nullable Project project, @NotNull ActionManager actionManager, IdeFocusManager focusManager, @NotNull Disposable parent) {
        super(project, parent);

        //inject is not supported here but we need SearchManagerHistory
        searchManager = PluginModule.getPluginModuleForProject(project).getInstance(SearchHistoryManager.class);

        addListener(new TabsListener() {
            @Override
            public void beforeSelectionChanged(TabInfo oldSelection, TabInfo newSelection) {
                //sync text and history
                if(searchFields.containsKey(oldSelection) && searchFields.containsKey(newSelection)){
                    searchFields.get(newSelection).setText(lastSearchText);
                    //sync history for newly open tabs
                    searchFields.get(newSelection).setHistory(searchFields.get(oldSelection).getHistory());
                }
            }

            @Override
            public void tabRemoved(TabInfo tabToRemove) {
                searchFields.remove(tabToRemove);
            }
        });

    }

    private CustomSearchTextField createTextField(){
        CustomSearchTextField newSearchTextField = new CustomSearchTextField();

        newSearchTextField.setHistorySize(5);
        newSearchTextField.addDocumentListener(new DocumentAdapter() {
            @Override
            protected void textChanged(DocumentEvent e) {
                lastSearchText = newSearchTextField.getText();
            }
        });

        newSearchTextField.setHistoryItemClickedHandler(()-> search(newSearchTextField));

        newSearchTextField.addKeyboardListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if(e.getKeyCode() == KeyEvent.VK_ENTER &&
                        searchRequestHandler != null &&
                        StringUtils.isNotBlank(newSearchTextField.getText())){

                    search(newSearchTextField);
                }
            }
        });

        return newSearchTextField;
    }

    private void search(CustomSearchTextField searchTextField){
        searchManager.addToSearchHistory(searchTextField.getText());
        //sync history
        searchFields.values().forEach(textField -> textField.setHistory(searchManager.getSearchHistory()));
        searchRequestHandler.searchedQuery(searchTextField.getText());
    }

    @NotNull
    @Override
    public TabInfo addTab(TabInfo info) {
        JPanel searchPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        CustomSearchTextField searchTextField = createTextField();
        searchPanel.add(searchTextField);
        searchFields.put(info, searchTextField);

        info.setSideComponent(searchPanel);
        return super.addTab(info);
    }

    public void setSearchRequestHandler(SearchRequestHandler searchRequestHandler){
        this.searchRequestHandler = searchRequestHandler;
    }

    public void setSearchHistory(List<String> searchHistory){
        searchFields.values().forEach(searchTextField -> searchTextField.setHistory(searchHistory));
    }


}