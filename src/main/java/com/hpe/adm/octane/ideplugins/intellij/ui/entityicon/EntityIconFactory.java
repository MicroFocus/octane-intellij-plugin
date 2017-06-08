/*
 * Copyright 2017 Hewlett-Packard Enterprise Development Company, L.P.
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

import com.hpe.adm.octane.ideplugins.intellij.ui.Constants;
import com.hpe.adm.octane.services.filtering.Entity;
import com.intellij.util.ImageLoader;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

public class EntityIconFactory {

    //Detail for unmapped entity type
    private final IconDetail unmapedEntityIconDetail = new IconDetail(new Color(0,0,0,0), "", true);

    //map to color and short text
    private final Map<Entity, IconDetail> iconDetailMap = new HashMap<>();
    private final Map<Entity, JComponent> iconComponentMap = new HashMap<>();

    private int iconHeight = 30;
    private int iconWidth = 30;
    private Color fontColor = new Color(255,255,255);
    private int fontSize = 15;

    private static final Image activeImg = ImageLoader.loadFromResource(Constants.IMG_ACTIVE_ITEM);
   
    public EntityIconFactory(){	
    	init();
    }
    
    public EntityIconFactory(int iconHeight, int iconWidth, int fontSize, Color fontColor){
    	this.iconHeight = iconHeight;
    	this.iconWidth = iconWidth;
    	this.fontColor = fontColor;
        this.fontSize = fontSize;
    	init();
    }

    public EntityIconFactory(int iconHeight, int iconWidth, int fontSize){
        this.iconHeight = iconHeight;
        this.iconWidth = iconWidth;
        this.fontSize = fontSize;
        init();
    }
    
    private void init(){
        iconDetailMap.put(Entity.USER_STORY, new IconDetail(new Color(218, 199, 120), "US"));
        iconDetailMap.put(Entity.QUALITY_STORY, new IconDetail(new Color(95, 112, 118), "QS"));
        iconDetailMap.put(Entity.DEFECT, new IconDetail(new Color(190,102,92), "D"));
        iconDetailMap.put(Entity.EPIC, new IconDetail(new Color(202, 170, 209), "E"));
        iconDetailMap.put(Entity.FEATURE, new IconDetail(new Color(226,132,90), "F"));

        iconDetailMap.put(Entity.TASK, new IconDetail(new Color(137,204,174), "T"));

        iconDetailMap.put(Entity.MANUAL_TEST, new IconDetail(new Color(96,121,141), "MT"));
        iconDetailMap.put(Entity.GHERKIN_TEST, new IconDetail(new Color(120,196,192), "GT"));

        iconDetailMap.put(Entity.TEST_SUITE, new IconDetail(new Color(133,114,147), "TS"));
        iconDetailMap.put(Entity.MANUAL_TEST_RUN, new IconDetail(new Color(133,169,188), "MR"));
        iconDetailMap.put(Entity.TEST_SUITE_RUN, new IconDetail(new Color(133,169,188), "SR"));
        iconDetailMap.put(Entity.AUTOMATED_TEST, new IconDetail(new Color(135,123,117), "AT"));

        iconDetailMap.put(Entity.COMMENT, new IconDetail(new Color(234, 179, 124), "C"));

        iconDetailMap.keySet().forEach(entity -> iconComponentMap.put(entity, createIconAsComponent(entity)));
    }
    
    private JComponent createIconAsComponent(Entity entity){
    	IconDetail iconDetail = iconDetailMap.containsKey(entity) ? iconDetailMap.get(entity) : unmapedEntityIconDetail;
    	
        //Make the label
        Font defaultFont = new JXLabel().getFont();
        Font boldFont = new Font(defaultFont.getFontName(), Font.BOLD, fontSize);

        JXLabel label = new JXLabel(iconDetail.getDisplayLabelText());
        
        label.setForeground(fontColor);
        label.setOpaque(iconDetail.isOpaque());
        label.setBackground(iconDetail.getColor());
        label.setPreferredSize(new Dimension(iconWidth, iconHeight));
        label.setMinimumSize(new Dimension(iconWidth, iconHeight));
        label.setMaximumSize(new Dimension(iconWidth, iconHeight));
        label.setFont(boldFont);
        label.setHorizontalAlignment(SwingConstants.CENTER);
        label.setVerticalAlignment(SwingConstants.CENTER);
        label.setBounds(0,0,iconWidth,iconHeight);
        return label;
    }

    private Image createIconAsImage(Entity entity, boolean isActive){
        JComponent lblIcon = getIconAsComponent(entity, isActive);
        lblIcon.setBounds(0,0,iconWidth,iconHeight);
        JFrame frame = new JFrame();
        frame.getContentPane().setLayout(null);
        frame.getContentPane().add(lblIcon);
        frame.pack();

        BufferedImage image = new BufferedImage(iconWidth, iconHeight, BufferedImage.TYPE_INT_ARGB);
        Graphics g = image.createGraphics();
        lblIcon.paint(g);

        return image;
    }

    public JComponent getIconAsComponent(Entity entity){
      	return iconComponentMap.get(entity);
    }

    public JComponent getIconAsComponent(Entity entity, boolean isActive){
        if(!isActive){
            return getIconAsComponent(entity);
        } else {
            //Overlay the run image on top of the original entity icon component
            JComponent component = getIconAsComponent(entity);

            if(component == null){
                return new JLabel("N/A");
            }

            component.setBounds(0,0,iconWidth,iconHeight);

            //Overlay an image on top of the component
            JPanel runImagePanel = new JPanel() {
                @Override
                public void paintComponent(Graphics g) {
                    super.paintComponent(g);
                    g.drawImage(activeImg, 0, 0, getWidth(), getHeight(), this);
                }
            };

            int xpercent = 60 * iconWidth / 100;
            int ypercent = 60 * iconWidth / 100;

            runImagePanel.setBounds(
                    xpercent,
                    ypercent,
                    iconWidth - xpercent,
                    iconHeight - ypercent);
            runImagePanel.setOpaque(false);

            JPanel panel = new JPanel(null);
            panel.setBorder(null);
            panel.setOpaque(false);
            panel.add(runImagePanel);
            panel.add(component);

            panel.setPreferredSize(new Dimension(iconWidth, iconHeight));
            panel.setMinimumSize(new Dimension(iconWidth, iconHeight));
            panel.setMaximumSize(new Dimension(iconWidth, iconHeight));
            return panel;
        }
    }

    public Image getIconAsImage(Entity entity, boolean isActive){
        return createIconAsImage(entity, isActive);
    }

    public Image getIconAsImage(Entity entity){
        return createIconAsImage(entity, false);
    }

}