package com.hpe.adm.octane.ideplugins.intellij.eventbus;

import com.google.common.eventbus.Subscribe;

public class RefreshMyWorkEvent {

    public interface RefreshMyWorkEventListener {
        @Subscribe
        void refresh(RefreshMyWorkEvent refreshMyWorkEvent);
    }

}
