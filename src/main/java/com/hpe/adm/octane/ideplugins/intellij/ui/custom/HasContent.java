package com.hpe.adm.octane.ideplugins.intellij.ui.custom;

import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;

/**
 * To supply intellij top level tool window content
 */
public abstract class HasContent implements HasComponent {
    public Content getContent(){
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        return contentFactory.createContent(getComponent(), "", false);
    }
}
