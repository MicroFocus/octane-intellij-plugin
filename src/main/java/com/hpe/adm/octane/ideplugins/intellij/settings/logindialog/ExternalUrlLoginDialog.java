package com.hpe.adm.octane.ideplugins.intellij.settings.logindialog;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class ExternalUrlLoginDialog extends LoginDialog {

    private static final Logger logger = Logger.getInstance(ExternalUrlLoginDialog.class.getName());
    private String loginPageUrl;

    public ExternalUrlLoginDialog(Project project, String loginPageUrl) {
        super(project, false, IdeModalityType.PROJECT);
        this.loginPageUrl = loginPageUrl;
        setTitle(TITLE);
        init();
    }

    @Nullable
    @Override
    protected JComponent createCenterPanel() {

        logger.debug("Showing login page for login url: " + loginPageUrl);

        JPanel contentPane = new JPanel();
        contentPane.setLayout(new BorderLayout(0, 10));

        JLabel lblOpenSystemBrowser = new JLabel("<html><a href=\\\"\\\">Please click here to use your system default browser to login to ALM Octane...</a></html>");
        lblOpenSystemBrowser.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        contentPane.add(lblOpenSystemBrowser, BorderLayout.CENTER);

        lblOpenSystemBrowser.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                try {
                    Desktop.getDesktop().browse(new URI(loginPageUrl));
                } catch (URISyntaxException | IOException ex) {
                    logger.error("Failed to open system browser, " + ex);
                }
            }
        });

        contentPane.setPreferredSize(new Dimension(800, -1));
        pack();
        centerRelativeToParent();
        return contentPane;
    }

}