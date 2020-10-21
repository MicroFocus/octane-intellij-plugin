package com.hpe.adm.octane.ideplugins.intellij.settings.logindialog;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;

public abstract class LoginDialog extends DialogWrapper {

    public static final String TITLE = "Login to ALM Octane";
    protected boolean wasClosed = false;

    protected LoginDialog(@Nullable Project project, boolean canBeParent) {
        super(project, canBeParent);
    }

    protected LoginDialog(@Nullable Project project, boolean canBeParent, @NotNull IdeModalityType ideModalityType) {
        super(project, canBeParent, ideModalityType);
    }

    protected LoginDialog(@Nullable Project project, @Nullable Component parentComponent, boolean canBeParent, @NotNull IdeModalityType ideModalityType) {
        super(project, parentComponent, canBeParent, ideModalityType);
    }

    protected LoginDialog(@Nullable Project project, @Nullable Component parentComponent, boolean canBeParent, @NotNull IdeModalityType ideModalityType, boolean createSouth) {
        super(project, parentComponent, canBeParent, ideModalityType, createSouth);
    }

    protected LoginDialog(@Nullable Project project) {
        super(project);
    }

    protected LoginDialog(boolean canBeParent) {
        super(canBeParent);
    }

    protected LoginDialog(Project project, boolean canBeParent, boolean applicationModalIfPossible) {
        super(project, canBeParent, applicationModalIfPossible);
    }

    protected LoginDialog(@NotNull Component parent, boolean canBeParent) {
        super(parent, canBeParent);
    }

    @NotNull
    @Override
    protected Action[] createActions() {
        return new Action[0];
    }

    @Override
    protected void dispose() {
        wasClosed = true;
        super.dispose();
    }

    public boolean wasClosed() {
        return wasClosed;
    }

}