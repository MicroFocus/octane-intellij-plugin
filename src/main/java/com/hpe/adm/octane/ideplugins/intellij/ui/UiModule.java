package com.hpe.adm.octane.ideplugins.intellij.ui;

import com.google.inject.AbstractModule;
import com.hpe.adm.octane.ideplugins.intellij.ConnectionSettingsConfigurable;
import com.hpe.adm.octane.ideplugins.intellij.ui.panels.ConnectionSettingsView;
import com.hpe.adm.octane.ideplugins.intellij.util.NotificationUtil;

public class UiModule extends AbstractModule {
    @Override
    protected void configure() {
        bind(ConnectionSettingsView.class);
        bind(ConnectionSettingsConfigurable.class);
        bind(NotificationUtil.class);
    }
}
