package com.hpe.adm.octane.ideplugins.intellij.ui.customcomponents;

import com.google.inject.Inject;
import com.hpe.adm.nga.sdk.model.EntityModel;
import com.hpe.adm.octane.services.util.Util;
import com.hpe.adm.octane.services.EntityService;
import com.hpe.adm.octane.services.exception.ServiceException;
import com.hpe.adm.octane.services.filtering.Entity;

import javax.swing.*;
import java.awt.*;
import java.util.Collection;

public class CustomComboBoxTester extends JFrame {

    @Inject
    private EntityService entityService;

    public CustomComboBoxTester() {
        super("Demo program for custom combobox");
        setLayout(new FlowLayout());

        PhaseComboBox customCombobox = new PhaseComboBox();
        customCombobox.setPreferredSize(new Dimension(120, 30));
        customCombobox.setEditable(true);
        Long entityId = 1072L;

        Collection<EntityModel> phaseList = null;
        try {
            EntityModel testEntityModel = entityService.findEntity(Entity.DEFECT, entityId);
            Long currentPhaseId = Long.valueOf(Util.getUiDataFromModel(testEntityModel.getValue("phase"), "id"));
            phaseList = entityService.findPossibleTransitionFromCurrentPhase(Entity.DEFECT, currentPhaseId);
        } catch (ServiceException e) {
            e.printStackTrace();
        }

        customCombobox.addItems(phaseList);

        add(customCombobox);

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(400, 100);
        setLocationRelativeTo(null);    // center on screen
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                new CustomComboBoxTester().setVisible(true);
            }
        });
    }
}