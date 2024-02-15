/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.settings.logindialog;

import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.LoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.util.JavaFxUtils;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.sun.javafx.application.PlatformImpl;
import javafx.concurrent.Worker.State;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;


public class JavaFxLoginDialog extends LoginDialog {

    private static final Logger logger = Logger.getInstance(JavaFxLoginDialog.class.getName());

    private String loginPageUrl;
    private boolean shouldUseJavaFxBrowser;
    private JPanel browserPanel;

    public JavaFxLoginDialog(Project project, String loginPageUrl) {
        this(project, loginPageUrl, true);
    }

    public JavaFxLoginDialog(Project project, String loginPageUrl, boolean shouldUseJavaFxBrowser) {
        super(project, false, IdeModalityType.IDE);
        this.loginPageUrl = loginPageUrl;
        this.shouldUseJavaFxBrowser = shouldUseJavaFxBrowser;
        setTitle(TITLE);
        init();
    }

    @Nullable
    @Override
    protected JComponent createCenterPanel() {

        logger.debug("Showing login page for login url: " + loginPageUrl);

        JPanel contentPane = new JPanel();
        contentPane.setLayout(new BorderLayout(0, 10));
        contentPane.setPreferredSize(new Dimension(800, 600));

        JLabel lblOpenSystemBrowser = new JLabel("<html>If the page below does not display correctly, <a href=\\\"\\\">click here to use your system default browser.</a></html>");
        lblOpenSystemBrowser.setBorder(new EmptyBorder(10, 0, 10, 0));
        lblOpenSystemBrowser.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        contentPane.add(lblOpenSystemBrowser, BorderLayout.NORTH);

        lblOpenSystemBrowser.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                try {
                    Desktop.getDesktop().browse(new URI(loginPageUrl));
                    JLabel lblSystemBrowser = new JLabel("Opening login page system browser, waiting for session...");
                    if(browserPanel != null) {
                        browserPanel.setVisible(false);
                    }
                    contentPane.add(lblSystemBrowser, BorderLayout.CENTER);
                    contentPane.setPreferredSize(new Dimension(800, -1));
                    JavaFxLoginDialog.this.pack();
                    JavaFxLoginDialog.this.centerRelativeToParent();

                } catch (URISyntaxException | IOException ex) {
                    logger.error("Failed to open system browser, " + ex);
                }
            }
        });

        // We need to check if the jvm running IntelliJ has java fx installed in it, on linux, openjdk can be installed without java fx
        if (JavaFxUtils.isJavaFxAvailable() && shouldUseJavaFxBrowser) {

            try {
                browserPanel = new JPanel(new BorderLayout());
                browserPanel.setBorder(new LineBorder(UIManager.getColor("Separator.foreground")));
                contentPane.add(browserPanel, BorderLayout.CENTER);

                LoadingWidget loadingWidget = new LoadingWidget("Loading login page...");
                JFXPanel jfxPanel = new JFXPanel();

                PlatformImpl.setImplicitExit(false);
                PlatformImpl.runAndWait(() -> {

                    Group root = new Group();
                    Stage stage = new Stage();
                    stage.setResizable(true);

                    Scene scene = new Scene(root, jfxPanel.getWidth(), jfxPanel.getHeight());
                    stage.setScene(scene);

                    // Set up the embedded browser:
                    WebView browser = new WebView();
                    WebEngine webEngine = browser.getEngine();

                    // process page loading
                    webEngine.getLoadWorker().stateProperty().addListener(
                            (ov, oldState, newState) ->
                                    SwingUtilities.invokeLater(() -> {
                                        browserPanel.remove(jfxPanel);
                                        browserPanel.remove(loadingWidget);

                                        if (State.SCHEDULED == newState || State.RUNNING == newState) {
                                            browserPanel.add(loadingWidget, BorderLayout.CENTER);
                                        } else {
                                            browserPanel.add(jfxPanel, BorderLayout.CENTER);
                                        }

                                        contentPane.repaint();
                                        contentPane.revalidate();
                                    })
                    );

                    webEngine.load(loginPageUrl);
                    root.getChildren().add(browser);

                    jfxPanel.setScene(scene);
                });
            } catch (Exception e) {
                logger.warn("Failed to load login page: " + e.getMessage());
            }

        } else {
            contentPane.setPreferredSize(new Dimension(800, -1));
            pack();
            centerRelativeToParent();
        }

        return contentPane;
    }

}