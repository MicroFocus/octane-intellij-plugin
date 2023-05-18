/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.html;

import com.google.inject.Inject;
import com.hpe.adm.octane.ideplugins.intellij.util.HtmlTextEditor;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.intellij.openapi.diagnostic.Logger;
import javafx.application.Platform;
import javafx.concurrent.Worker.State;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

public class JavaFxHtmlPanel extends HtmlPanel {

    private static final Logger log = Logger.getInstance(JavaFxHtmlPanel.class);

    private static final String EVENT_TYPE_CLICK = "click";
    private static final String EVENT_TYPE_MOUSEOVER = "mouseover";
    private static final String EVENT_TYPE_MOUSEOUT = "mouseclick";
    private static final String HYPERLINK_TAG = "a";

    private String htmlContent;

    private JFXPanel jfxPanel;
    private WebView webView;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    public JavaFxHtmlPanel() {

        setLayout(new BorderLayout());
        jfxPanel = new JFXPanel();
        add(jfxPanel, BorderLayout.CENTER);

        UIManager.addPropertyChangeListener(evt -> {
            if ("lookAndFeel".equals(evt.getPropertyName())) {
                Platform.runLater(() -> setHtmlContent(htmlContent));
            }
        });

        jfxPanel.addMouseWheelListener(e -> {
                Component source = (Component) e.getSource();
                MouseEvent parentEvent = SwingUtilities.convertMouseEvent(source, e, source.getParent());
                source.getParent().dispatchEvent(parentEvent);
            });

        setFocusable(false);
        addEventActions();
    }

    @Override
    public void setHtmlContent(final String htmlContent) {
        this.htmlContent = htmlContent;

        Platform.runLater(() -> {
            initFX();

            final String strippedContent = HtmlTextEditor.removeHtmlStructure(htmlContent);
            //final String coloredHtmlCode = HtmlTextEditor.getColoredHTML(strippedContent);

            final StackPane root = new StackPane();
            final Scene scene = new Scene(root);
            final WebView webView = getWebView();
            final WebEngine webEngine = webView.getEngine();

            root.getChildren().add(webView);

            jfxPanel.setScene(scene);
            webEngine.loadContent(strippedContent);
            Platform.setImplicitExit(false);

        });
    }

    @Override
    public String getHtmlContent() {
        return htmlContent;
    }

    private void addHyperlinkListener(HyperlinkListener listener) {
        listenerList.add(HyperlinkListener.class, listener);
    }

    public void removeHyperlinkListener(HyperlinkListener listener) {
        listenerList.remove(HyperlinkListener.class, listener);
    }

    private void fireHyperlinkUpdate(HyperlinkEvent.EventType eventType, String desc) {

        HyperlinkEvent event = new HyperlinkEvent(this, eventType, null, desc);
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == HyperlinkListener.class) {
                ((HyperlinkListener) listeners[i + 1]).hyperlinkUpdate(event);
            }
        }
    }

    /**
     * Initialization of the FX component. Gets a webview if one exists or creates a new one.
     * From the webview object, the page engine is fetched together with it's worker(object
     * which keeps track of the progress of a task (FAILED, RUNNING, SUCCEEDED). A hyperlink
     * listener is then added to SUCCEEDED state property.
     * <p>
     * When a new state was succesfully transitioned to, mouse events are caught and handled accordingly.
     * P.S. For this use case mouseover and mouseout event were not required to be handled.
     */
    private void initFX() {

        webView = getWebView();

        webView.getEngine().getLoadWorker().stateProperty().addListener((ov, oldState, newState) -> {
            if (newState == State.SUCCEEDED) {
                EventListener listener = ev -> {
                    String href = null;
                    String domEventType = ev.getType();
                    final HyperlinkEvent.EventType eventType;

                    switch (domEventType) {
                        case EVENT_TYPE_CLICK:
                            eventType = HyperlinkEvent.EventType.ACTIVATED;
                            ev.preventDefault();
                            href = ev.getCurrentTarget().toString();
                            break;
                        case EVENT_TYPE_MOUSEOVER:
                            //To implement actions if needed
                            eventType = HyperlinkEvent.EventType.ENTERED;
                            break;
                        case EVENT_TYPE_MOUSEOUT:
                            //To implement actions if needed
                            eventType = HyperlinkEvent.EventType.EXITED;
                            break;
                        default:
                            return;
                    }

                    if (href != null) {
                        fireHyperlinkUpdate(eventType, href);
                    }

                };
                final Document doc = webView.getEngine().getDocument();
                final NodeList nodeList = doc.getElementsByTagName(HYPERLINK_TAG);
                //Foreach not applicable for NodeList class
                for (int i = 0; i < nodeList.getLength(); i++) {
                    EventTarget eventTarget = (EventTarget) nodeList.item(i);
                    addEventListeners(eventTarget, listener);
                }
            }
        });
    }

    private void addEventListeners(EventTarget eventTarget, EventListener listener) {
        eventTarget.addEventListener(EVENT_TYPE_CLICK, listener, false);
        eventTarget.addEventListener(EVENT_TYPE_MOUSEOVER, listener, false);
        eventTarget.addEventListener(EVENT_TYPE_MOUSEOUT, listener, false);
    }


    private void addEventActions() {
        this.addHyperlinkListener(evt -> {
            final String href = evt.getDescription();
            URL targetUrl;
            URI targetUri;

            HyperlinkEvent.EventType eventType = evt.getEventType();

            try {
                //useful for checking whether relative or absolute url been given
                try {
                    targetUrl = new URL(href);
                } catch (MalformedURLException ex) {
                    targetUrl = new URL(connectionSettingsProvider.getConnectionSettings().getBaseUrl() + href);
                }
                targetUri = targetUrl.toURI();
                if (HyperlinkEvent.EventType.ACTIVATED.equals(eventType)) {
                    Desktop.getDesktop().browse(targetUri);
                } else if (HyperlinkEvent.EventType.ENTERED.equals(eventType)) {
                    //To be implemented if needed
                } else if (HyperlinkEvent.EventType.EXITED.equals(eventType)) {
                    //To be implemented if needed
                }
            } catch (URISyntaxException | IOException e) {
                log.error("URL parsing exception ", e);
            }
        });
    }

    private WebView getWebView() {
        if (webView == null) {
            webView = new WebView();
        }

        return webView;
    }

}