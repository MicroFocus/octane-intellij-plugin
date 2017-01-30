package com.hpe.adm.octane.ideplugins.intellij.eventbus;

import com.google.common.eventbus.Subscribe;

public interface OpenDetailTabEventListener {
    @Subscribe
    void openDetailTab(OpenDetailTabEvent openDetailTabEvent);
}
