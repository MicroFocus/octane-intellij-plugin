package com.hpe.adm.octane.ideplugins.intellij.ui.entityicon;

import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
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
    private final Map<Entity, Image> iconImageMap = new HashMap<>();
    private int iconHeight = 30;
    private int iconWidth = 30;
    private Color fontColor = new Color(255,255,255);
    private int fontSize = 15;
   
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
        iconDetailMap.put(Entity.DEFECT, new IconDetail(new Color(190,102,92), "D"));
        iconDetailMap.put(Entity.TASK, new IconDetail(new Color(137,204,174), "T"));
        iconDetailMap.put(Entity.MANUAL_TEST, new IconDetail(new Color(96,121,141), "MT"));
        iconDetailMap.put(Entity.GHERKIN_TEST, new IconDetail(new Color(120,196,192), "GT"));
        
        iconDetailMap.keySet().forEach(entity -> iconComponentMap.put(entity, createIconAsComponent(entity)));
        iconComponentMap.keySet().forEach(entity -> iconImageMap.put(entity, createIconAsImage(entity)));
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

        return label;
    }

    private Image createIconAsImage(Entity entity){
        JComponent lblIcon = getIconAsComponent(entity);
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
    public Image getIconAsImage(Entity entity){
        return iconImageMap.get(entity);
    }

}