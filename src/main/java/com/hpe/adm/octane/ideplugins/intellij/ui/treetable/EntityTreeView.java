package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.View;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.PacmanLoadingWidget;
import com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents.tree.FillingTree;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.nowork.NoWorkPanel;
import com.hpe.adm.octane.ideplugins.intellij.ui.util.UiUtil;
import com.hpe.adm.octane.ideplugins.intellij.util.Constants;
import com.hpe.adm.octane.ideplugins.intellij.util.RestUtil;
import com.hpe.adm.octane.ideplugins.services.DownloadScriptService;
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.util.UrlParser;
import com.intellij.icons.AllIcons;
import com.intellij.ide.DataManager;
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.*;
import java.net.URI;
import java.nio.charset.StandardCharsets;

public class EntityTreeView implements View {

    public interface EntityDoubleClickHandler {
        void entityDoubleClicked(Entity entityType, Long entityId, EntityModel model);
    }

    private JPanel rootPanel;

    private FillingTree tree;
    private JBScrollPane scrollPane;

    private JButton refreshButton;

    @Inject
    private DownloadScriptService scriptService;

    @Inject
    private UrlParser urlParser;

    @Inject
    private ConnectionSettingsProvider connectionSettingsProvider;

    public EntityTreeView() {

        tree = new FillingTree();
        //Init with an empty model
        tree.setModel(new EntityTreeModel());

        tree.setRootVisible(false);
        tree.setCellRenderer(new EntityTreeCellRenderer());

        //Add it to the root panel
        rootPanel = new JPanel(new BorderLayout(0, 0));
        scrollPane = new JBScrollPane();
        scrollPane.setBorder(BorderFactory.createEmptyBorder());

        rootPanel.add(scrollPane, BorderLayout.CENTER);
        scrollPane.setViewportView(tree);

        //Toolbar
        rootPanel.add(createToolbar(), BorderLayout.EAST);

        tree.addMouseListener(createTreeContextMenu());
    }

    public void addEntityDoubleClickHandler(EntityDoubleClickHandler handler) {
        tree.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e)) {
                    int selRow = tree.getRowForLocation(e.getX(), e.getY());
                    TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
                    if (selRow != -1 && e.getClickCount() == 2) {
                        Object object = selPath.getLastPathComponent();
                        if (object instanceof EntityModel) {
                            try {
                                EntityModel entityModel = (EntityModel) object;
                                Entity entityType = Entity.getEntityType(entityModel);
                                Long entityId = Long.parseLong(entityModel.getValue("id").getValue().toString());
                                handler.entityDoubleClicked(entityType, entityId, entityModel);
                            } catch (Exception ex) {
                                System.out.print(ex);
                            }
                        }
                    }
                }
            }
        });
    }

    private VirtualFile chooseScriptFolder(Project project) {
        FileChooserDescriptor descriptor = new OpenProjectFileChooserDescriptor(true);
        descriptor.setHideIgnored(false);
        descriptor.setRoots(project.getBaseDir());
        descriptor.setTitle("Select Parent Folder");
        descriptor.withTreeRootVisible(false);
        VirtualFile[] virtualFile = FileChooser.chooseFiles(descriptor, null, null);

        return (virtualFile.length != 1) ? null : virtualFile[0];
    }

    private File createTestScriptFile(String path, String fileName, String script) {
        File f = new File(path + "/" + fileName);
        try {
            f.createNewFile();
            if (script != null) {
                Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f), StandardCharsets.UTF_8));
                out.append(script);
                out.flush();
                out.close();
            }
        } catch (IOException e) {
            return null;
        }
        return f;
    }

    private void downloadScriptForGherkinTest(long gherkinTestId) {
        DataContext dataContext = DataManager.getInstance().getDataContext();
        Project project = DataKeys.PROJECT.getData(dataContext);

        VirtualFile selectedFolder = chooseScriptFolder(project);
        if (selectedFolder != null) {
            RestUtil.LOADING_MESSAGE = "Downloading script for gherkin test with id " + gherkinTestId;
            RestUtil.runInBackground(
                    () -> {
                        String scriptContent = scriptService.getGherkinTestScriptContent(gherkinTestId);
                        String scriptFileName = "test #" + gherkinTestId + " script";
                        return createTestScriptFile(selectedFolder.getPath(), scriptFileName, scriptContent);
                    },
                    (scriptFile) -> {
                        VirtualFile vFile = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(scriptFile);
                        FileEditorManager.getInstance(project).openFile(vFile, true, true);
                        project.getBaseDir().refresh(false, true);
                    },
                    project,
                    "failed to download script for gherkin test with id " + gherkinTestId);
        }
    }


    private MouseListener createTreeContextMenu() {
        MouseListener ml = new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                TreePath path = tree.getPathForLocation(e.getX(), e.getY());

                if (SwingUtilities.isRightMouseButton(e) && path != null) {
                    Object obj = path.getLastPathComponent();

                    if (obj instanceof EntityModel) {
                        EntityModel entityModel = (EntityModel) obj;
                        Entity entityType = Entity.getEntityType(entityModel);

                        JPopupMenu popup = new JPopupMenu();

                        JMenuItem viewInBrowserItem = new JMenuItem("View in browser", IconLoader.findIcon(Constants.IMG_BROWSER_ICON));
                        viewInBrowserItem.addMouseListener(new MouseAdapter() {
                            @Override
                            public void mousePressed(MouseEvent mouseEvent) {
                                Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
                                if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {
                                    try {
                                        URI uri =
                                                UrlParser.createEntityWebURI(
                                                        connectionSettingsProvider.getConnectionSettings(),
                                                        entityType,
                                                        Integer.valueOf(UiUtil.getUiDataFromModel(entityModel.getValue("id")))) ;

                                        desktop.browse(uri);
                                    } catch (Exception ex) {
                                        ex.printStackTrace();
                                    }
                                }
                            }
                        });
                        popup.add(viewInBrowserItem);

                        if (entityType == Entity.GHERKIN_TEST) {
                            long id = Long.parseLong(entityModel.getValue("id").getValue().toString());
                            JMenuItem downloadScriptItem = new JMenuItem("Download script", AllIcons.Actions.Download);
                            downloadScriptItem.addMouseListener(new MouseAdapter() {
                                @Override
                                public void mousePressed(MouseEvent e) {
                                    super.mousePressed(e);
                                    if (SwingUtilities.isLeftMouseButton(e))
                                        downloadScriptForGherkinTest(id);
                                }
                            });
                            popup.add(downloadScriptItem);
                        }

                        //TODO implement me please
                        popup.addSeparator();
                        popup.add(new JMenuItem("Start work", IconLoader.findIcon(Constants.IMG_START_TIMER)));
                        popup.add(new JMenuItem("Stop work", IconLoader.findIcon(Constants.IMG_STOP_TIMER)));

                        popup.show(e.getComponent(), e.getX(), e.getY());
                    }
                }
            }
        };
        return ml;
    }

    private DefaultActionGroup toolBarActionGroup = new DefaultActionGroup();

    public void addActionToToolbar(AnAction action){
        toolBarActionGroup.addAction(action);
    }

    public void addSeparatorToToolbar(){
        toolBarActionGroup.addSeparator();
    }

    private JComponent createToolbar() {
        ActionToolbar actionToolBar = ActionManager.getInstance().createActionToolbar("My Work actions", toolBarActionGroup, false);
        actionToolBar.getComponent().setBorder(BorderFactory.createMatteBorder(0,1,0,0, JBColor.border()));
        return actionToolBar.getComponent();
    }

    public void expandAllNodes() {
        int j = tree.getRowCount();
        int i = 0;
        while (i < j) {
            tree.expandRow(i);
            i += 1;
            j = tree.getRowCount();
        }
    }

    public void collapseAllNodes() {
        int j = tree.getRowCount();
        int i = 0;
        while (i < j) {
            tree.collapseRow(i);
            i += 1;
            j = tree.getRowCount();
        }
    }

    @Override
    public JComponent getComponent() {
        return rootPanel;
    }

    private PacmanLoadingWidget loadingWidget = new PacmanLoadingWidget("Loading...");

    public void setLoading(boolean isLoading) {
        SwingUtilities.invokeLater(() -> {
            if (isLoading) {
                rootPanel.remove(scrollPane);
                rootPanel.add(loadingWidget, BorderLayout.CENTER);
            } else {
                rootPanel.remove(loadingWidget);
                rootPanel.add(scrollPane);
            }
            rootPanel.revalidate();
            rootPanel.repaint();
        });
    }

    public void setTreeModel(EntityTreeModel model) {
        tree.setModel(model);

        if(model.size() == 0){
            scrollPane.setViewportView(new NoWorkPanel());
        } else if(scrollPane.getViewport().getView() != tree){
            scrollPane.setViewportView(tree);
        }
    }

    public EntityTreeModel getTreeModel() {
        return (EntityTreeModel) tree.getModel();
    }


}
