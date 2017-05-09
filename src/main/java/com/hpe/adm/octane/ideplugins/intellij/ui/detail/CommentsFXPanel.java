package com.hpe.adm.octane.ideplugins.intellij.ui.detail;

import com.intellij.openapi.diagnostic.Logger;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.web.WebView;
import javafx.concurrent.Worker.State;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import java.awt.*;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

public class CommentsFXPanel extends JFXPanel {
    private static final Logger log = Logger.getInstance(CommentsFXPanel.class);
    private static final String EVENT_TYPE_CLICK = "click";
    private static final String EVENT_TYPE_MOUSEOVER = "mouseover";
    private static final String EVENT_TYPE_MOUSEOUT = "mouseclick";
    private static final String HYPERLINK_TAG = "a";
    private WebView webView;

    CommentsFXPanel() {
        Platform.runLater(this::initFX);
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
     *
     * When a new state was succesfully transitioned to, mouse events are caught and handled accordingly.
     * P.S. For this use case mouseover and mouseout event were not required to be handled.
     */
    void initFX() {
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

    private void addEventListeners(EventTarget eventTarget, EventListener listener){
        eventTarget.addEventListener(EVENT_TYPE_CLICK, listener, false);
        eventTarget.addEventListener(EVENT_TYPE_MOUSEOVER, listener, false);
        eventTarget.addEventListener(EVENT_TYPE_MOUSEOUT, listener, false);
    }

    public void addEventActions() {
        this.addHyperlinkListener(evt -> {
            HyperlinkEvent.EventType eventType = evt.getEventType();

            String href = evt.getDescription();

            URL targetUrl;
            URI targetUri;

            try {
                targetUrl = new URL(href);
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

    WebView getWebView() {
        if (webView == null) {
            webView = new WebView();
        }

        return webView;
    }

    void setWebView(WebView webView) {
        this.webView = webView;
    }
}
