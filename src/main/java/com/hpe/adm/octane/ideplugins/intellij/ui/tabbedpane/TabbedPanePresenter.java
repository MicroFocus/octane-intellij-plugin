package com.hpe.adm.octane.ideplugins.intellij.ui.tabbedpane;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.protobuf.ServiceException;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.ideplugins.intellij.ui.Presenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailPresenter;
import com.hpe.adm.octane.ideplugins.intellij.ui.detail.EntityDetailView;
import com.hpe.adm.octane.ideplugins.intellij.ui.treetable.EntityTreeTablePresenter;
import com.hpe.adm.octane.ideplugins.services.DownloadScriptService;
import com.hpe.adm.octane.ideplugins.services.EntityService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.hpe.adm.octane.ideplugins.services.filtering.Filter;
import com.intellij.ide.DataManager;
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.DataKeys;
import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

public class TabbedPanePresenter implements Presenter<TabbedPaneView> {

    @Inject
    TabbedPaneView tabbedPaneView;
    @Inject
    EntityService entityService;
    @Inject
    DownloadScriptService scriptService;

    @Inject
    Provider<EntityDetailPresenter> entityDetailPresenterProvider;
    @Inject
    Provider<EntityTreeTablePresenter> entityTreeTablePresenterProvider;

    List<EntityDetailPresenter> detailPresenters = new ArrayList<>();
    List<EntityTreeTablePresenter> treeTablePresenters = new ArrayList<>();

    public EntityTreeTablePresenter openMyWorkTab(String tabName) {
        EntityTreeTablePresenter presenter = entityTreeTablePresenterProvider.get();
        treeTablePresenters.add(presenter);
        tabbedPaneView.addTab(tabName, presenter.getView().getComponent());
        return presenter;
    }

    public void openDetailTab(EntityModel entityModel) {
        EntityDetailPresenter presenter = entityDetailPresenterProvider.get();
        detailPresenters.add(presenter);
        presenter.setEntity(entityModel);
        tabbedPaneView.addTab(EntityDetailView.getNameForEntity(Entity.getEntityType(entityModel)) + " | " + entityModel.getValue("id").getValue().toString(), presenter.getView().getComponent());
    }

    public void openSearchTab(String searchQuery) {
        //TODO: impl me
    }

    public void openFilteredTab(String tabName, List<Filter> filters) {
        //TODO: impl me
    }

    public TabbedPaneView getView() {
        return tabbedPaneView;
    }

    @Override
    @Inject
    public void setView(TabbedPaneView tabbedPaneView) {
        this.tabbedPaneView = tabbedPaneView;
        //open test entity tree view
        EntityTreeTablePresenter presenter = openMyWorkTab("My work");
        presenter.addEntityDoubleClickHandler((entityType, entityId, model) -> {
            //openDetailTab(model); //would still work, but it will be partially loaded in the future
            try {
                openDetailTab(entityService.findEntity(entityType, entityId));
            } catch (ServiceException ex){
                ex.printStackTrace();
            }
        });

        try {
            openDetailTab(entityService.findEntity(Entity.USER_STORY, Long.valueOf(159001)));
            openDetailTab(entityService.findEntity(Entity.TASK, Long.valueOf(52003)));
            openDetailTab(entityService.findEntity(Entity.TEST, Long.valueOf(231018)));
            openDetailTab(entityService.findEntity(Entity.DEFECT, Long.valueOf(163015)));
        } catch (ServiceException e) {
            e.printStackTrace();
        }
        openTestTab(1003);
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

    private void openTestTab(int testId) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        JButton button = new JButton("Test get script");
        panel.add(button);
        button.addActionListener((event) -> {
            String scriptContent = scriptService.getGherkinTestScriptContent(testId);
            if (scriptContent != null) {
                DataContext dataContext = DataManager.getInstance().getDataContext();
                Project project = DataKeys.PROJECT.getData(dataContext);
                VirtualFile selectedFolder = chooseScriptFolder(project);
                if (selectedFolder != null) {
                    String scriptFileName = "test #" + testId + " script";
                    File scriptFile = createTestScriptFile(selectedFolder.getPath(), scriptFileName, scriptContent);

                    VirtualFile vFile = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(scriptFile);
                    FileEditorManager.getInstance(project).openFile(vFile, true, true);
                    project.getBaseDir().refresh(false, true);
                }
            }
        });
        tabbedPaneView.addTab("TEST TAB", panel);
    }

    private void openDummyDetailTab(Entity entity, EntityModel entityModel) {
        //assume it has a name at least
        String name = entity.name() + ": " + entityModel.getValue("name").getValue().toString();
        tabbedPaneView.addTab(name, new JLabel(entityModel.toString()));
    }

}
