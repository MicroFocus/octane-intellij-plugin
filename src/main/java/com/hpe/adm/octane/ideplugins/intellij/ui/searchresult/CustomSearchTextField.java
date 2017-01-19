package com.hpe.adm.octane.ideplugins.intellij.ui.searchresult;

import com.intellij.ui.SearchTextField;

import javax.swing.*;

/**
 * Will notify you if the user selected something from the history
 */
public class CustomSearchTextField extends SearchTextField{

    protected Runnable createItemChosenCallback(final JList list) {
        return () -> {
            final String value = (String)list.getSelectedValue();
            getTextEditor().setText(value != null ? value : "");
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
