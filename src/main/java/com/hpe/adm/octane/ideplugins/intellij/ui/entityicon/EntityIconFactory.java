/*
 * © 2017 EntIT Software LLC, a Micro Focus company, L.P.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.hpe.adm.octane.ideplugins.intellij.ui.entityicon;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.ideplugins.services.EntityLabelService;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.util.ImageLoader;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

/*
JBColor might not be backwards compatible with prev intellij versions
 */
@SuppressWarnings({"UseJBColor", "InspectionUsingGrayColors"})
@Singleton
public class EntityIconFactory {

    @Inject
    private EntityLabelService entityLabelService;

    private final Color fontColor = new Color(255, 255, 255);

    private final Color unmappedEntityIconColor = new Color(0, 0, 0);

    private static final Map<Entity, Color> entityColorMap = new HashMap<>();
    static {
        entityColorMap.put(Entity.USER_STORY, new Color(255, 176, 0));
        entityColorMap.put(Entity.QUALITY_STORY, new Color(51, 193, 128));
        entityColorMap.put(Entity.DEFECT, new Color(178, 22, 70));
        entityColorMap.put(Entity.EPIC, new Color(116, 37, 173));
        entityColorMap.put(Entity.FEATURE, new Color(229, 120, 40));
        entityColorMap.put(Entity.TASK, new Color(22, 104, 193));
        entityColorMap.put(Entity.MANUAL_TEST, new Color(0, 171, 243));
        entityColorMap.put(Entity.GHERKIN_TEST, new Color(0, 169, 137));
        entityColorMap.put(Entity.TEST_SUITE, new Color(39, 23, 130));
        entityColorMap.put(Entity.MANUAL_TEST_RUN, new Color(0, 171, 243));
        entityColorMap.put(Entity.TEST_SUITE_RUN, new Color(0, 171, 243));
        entityColorMap.put(Entity.AUTOMATED_TEST, new Color(186, 71, 226));
        entityColorMap.put(Entity.COMMENT, new Color(253, 225, 89));
        entityColorMap.put(Entity.REQUIREMENT, new Color(11, 142, 172));
    }

    //cache the icons based on size, font and entity
    private Map<Integer, Map<Integer, Map<Entity, JComponent>>> iconComponentMap = new HashMap<>();

    private static final Image activeImg = ImageLoader.loadFromResource(Constants.IMG_ACTIVE_ITEM);

    @Inject
    public EntityIconFactory(EntityLabelService entityLabelService) {
        this.entityLabelService = entityLabelService;
    }

    private JComponent createIconAsComponent(Entity entity, int iconSize, int fontSize) {
        //Make the label
        JXLabel label = new JXLabel(new ImageIcon(createIconAsImage(entity, iconSize, fontSize)));
        label.setPreferredSize(new Dimension(iconSize, iconSize));
        label.setMinimumSize(new Dimension(iconSize, iconSize));
        label.setMaximumSize(new Dimension(iconSize, iconSize));
        label.setHorizontalAlignment(SwingConstants.CENTER);
        label.setVerticalAlignment(SwingConstants.CENTER);
        label.setBounds(0, 0, iconSize, iconSize);
        return label;
    }

    private Image createIconAsImage(Entity entity, int iconSize, int fontSize) {

        Color iconColor = entityColorMap.getOrDefault(entity, unmappedEntityIconColor);
        String iconText =  entityLabelService.getEntityInitials(entity);

        BufferedImage image = new BufferedImage(iconSize, iconSize, BufferedImage.TYPE_INT_ARGB);
        Graphics2D bg = image.createGraphics();
        // make BufferedImage fully transparent
        bg.setComposite(AlphaComposite.Clear);
        bg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        bg.fillRect(0, 0, iconSize, iconSize);
        bg.setComposite(AlphaComposite.SrcOver);
        bg.setColor(iconColor);

        bg.fillOval(0, 0, iconSize, iconSize);
        bg.setColor(fontColor);
        bg.setFont(new Font("Arial", Font.BOLD, fontSize));

        FontMetrics fm = bg.getFontMetrics();
        int fontX = (iconSize - fm.stringWidth(iconText) + 1) / 2;
        int fontY = (fm.getAscent() + (iconSize - (fm.getAscent() + fm.getDescent())) / 2);

        bg.drawString(iconText, fontX, fontY);

        return image;
    }

    public JComponent getIconAsComponent(Entity entity, int iconSize, int fontSize) {
        if (iconComponentMap.get(iconSize) == null) {
            //create a map for this size
            Map<Entity, JComponent> componentMap = new HashMap<>();
            componentMap.put(entity, createIconAsComponent(entity, iconSize, fontSize));
            Map<Integer, Map<Entity, JComponent>> fontSizeMap = new HashMap<>();
            fontSizeMap.put(fontSize, componentMap);
            iconComponentMap.put(iconSize, fontSizeMap);
        } else if (iconComponentMap.get(iconSize).get(fontSize) == null) {
            //size map exists but fontSize map does not
            Map<Entity, JComponent> componentMap = new HashMap<>();
            componentMap.put(entity, createIconAsComponent(entity, iconSize, fontSize));
            iconComponentMap.get(iconSize).put(fontSize, componentMap);
        } else if (iconComponentMap.get(iconSize).get(fontSize).get(entity) == null) {
            //size map exists, font map exists but entity icon does not
            iconComponentMap.get(iconSize).get(fontSize).put(entity, createIconAsComponent(entity, iconSize, fontSize));
        }

        return iconComponentMap.get(iconSize).get(fontSize).get(entity);
    }

    public JComponent getIconAsComponent(Entity entity, int iconSize, int fontSize, boolean isActive) {
        if (!isActive) {
            return getIconAsComponent(entity, iconSize, fontSize);
        } else {
            //Overlay the run image on top of the original entity icon component
            JComponent component = getIconAsComponent(entity, iconSize, fontSize);

            if (component == null) {
                return new JLabel("N/A");
            }

            component.setBounds(0, 0, iconSize, iconSize);

            //Overlay an image on top of the component
            JPanel runImagePanel = new JPanel() {
                @Override
                public void paintComponent(Graphics g) {
                    super.paintComponent(g);
                    g.drawImage(activeImg, 0, 0, getWidth(), getHeight(), this);
                }
            };

            int xpercent = 60 * iconSize / 100;
            int ypercent = 60 * iconSize / 100;

            runImagePanel.setBounds(
                    xpercent,
                    ypercent,
                    iconSize - xpercent,
                    iconSize - ypercent);
            runImagePanel.setOpaque(false);

            JPanel panel = new JPanel(null);
            panel.setBorder(null);
            panel.setOpaque(false);
            panel.add(runImagePanel);
            panel.add(component);

            panel.setPreferredSize(new Dimension(iconSize, iconSize));
            panel.setMinimumSize(new Dimension(iconSize, iconSize));
            panel.setMaximumSize(new Dimension(iconSize, iconSize));
            return panel;
        }
    }


    public Image getIconAsImage(Entity entity, int iconSize, int fontSize) {
        return createIconAsImage(entity, iconSize, fontSize);
    }

}