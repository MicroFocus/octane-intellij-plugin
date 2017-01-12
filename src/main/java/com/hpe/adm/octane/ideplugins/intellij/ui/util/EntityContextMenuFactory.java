package com.hpe.adm.octane.ideplugins.intellij.ui.util;


import com.hpe.adm.nga.sdk.model.EntityModel;

import javax.swing.*;

public interface EntityContextMenuFactory {
    JPopupMenu createContextMenu(EntityModel entityModel);
}
