package com.hpe.adm.octane.ideplugins.intellij.settings;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.util.ui.JBUI;
import com.sun.javafx.application.PlatformImpl;
import com.sun.javafx.webkit.WebConsoleListener;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class LoginDialog extends DialogWrapper {

    private String loginPageUrl;

    public LoginDialog(Project project) {
        super(project, false, IdeModalityType.PROJECT);
        setSize(800, 600); //default
        setTitle("Octane IntelliJ Plugin: Login");
        init();
    }

    @Nullable
    @Override
    protected JComponent createCenterPanel() {
        JPanel contentPane = new JPanel();
        contentPane.setLayout(new BorderLayout(0, 10));
        contentPane.setPreferredSize(new Dimension(800, 600));

        JLabel lblOpenSystemBrowser = new JLabel("<html>Login to ALM Octane<br>If the page below does not display correctly, <a href=\\\"\\\">click here to use your system default browser.</a></html>");
        lblOpenSystemBrowser.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        contentPane.add(lblOpenSystemBrowser, BorderLayout.NORTH);

        JFXPanel jfxPanel = new JFXPanel();
        jfxPanel.setBorder(new LineBorder(UIManager.getColor("Separator.foreground")));
        contentPane.add(jfxPanel, BorderLayout.CENTER);

        PlatformImpl.runLater(() -> {

            Group root = new Group();

            WebConsoleListener.setDefaultListener(new WebConsoleListener(){
                @Override
                public void messageAdded(WebView webView, String message, int lineNumber, String sourceId) {
                    System.out.println("Console: [" + sourceId + ":" + lineNumber + "] " + message);
                }
            });

            Stage stage = new Stage();
            stage.setResizable(true);

            Scene scene = new Scene(root, jfxPanel.getWidth(), jfxPanel.getHeight());
            stage.setScene(scene);

            // Set up the embedded browser:
            WebView browser = new WebView();
            WebEngine webEngine = browser.getEngine();
            webEngine.load(loginPageUrl);
            root.getChildren().add(browser);


            jfxPanel.setScene(scene);
        });

        lblOpenSystemBrowser.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                try {
                    Desktop.getDesktop().browse(new URI(loginPageUrl));
                    JLabel lblSystemBrowser = new JLabel("Opening login page system browser, waiting for session...");
                    jfxPanel.setVisible(false);
                    contentPane.add(lblSystemBrowser, BorderLayout.CENTER);

                } catch (URISyntaxException | IOException ex) {
                    //It looks like there's a problem
                }
            }
        });

        return contentPane;
    }

    @NotNull
    @Override
    protected Action[] createActions() {return new Action[0];}

    public void setLoginPageUrl(String loginPageUrl) {
        this.loginPageUrl = loginPageUrl;
    }
}