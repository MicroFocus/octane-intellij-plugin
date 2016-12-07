package com.hpe.adm.octane.ideplugins.intellij.ui.entityicon;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.SwingConstants;

import org.jdesktop.swingx.JXLabel;

import com.hpe.adm.octane.ideplugins.services.filtering.Entity;

public class EntityIconFactory {

    private int iconHeight = 30;
    private int iconWidth = 30;
    private Color fontColor = new Color(255,255,255);
    private int fontSize = 15;

    //Detail for unmapped entity type
    private final IconDetail unmapedEntityIconDetail = new IconDetail(new Color(0,0,0,0), "", true);

    //map to color and short text
    private final Map<Entity, IconDetail> iconDetailMap = new HashMap<>();
  
    private final Map<Entity, JComponent> iconMap = new HashMap<>();
   
    public EntityIconFactory(){	
    	init();
    }
    
    public EntityIconFactory(int iconHeight, int iconWidth, Color fontColor){
    	this.iconHeight = iconHeight;
    	this.iconWidth = iconWidth;
    	this.fontColor = fontColor;
    	init();
    }
    
    private void init(){
        iconDetailMap.put(Entity.USER_STORY, new IconDetail(new Color(218, 199, 120), "US"));
        iconDetailMap.put(Entity.DEFECT, new IconDetail(new Color(190,102,92), "D"));
        iconDetailMap.put(Entity.TASK, new IconDetail(new Color(137,204,174), "T"));
        iconDetailMap.put(Entity.MANUAL_TEST, new IconDetail(new Color(96,121,141), "MT"));
        iconDetailMap.put(Entity.GHERKIN_TEST, new IconDetail(new Color(120,196,192), "GT"));
        
        iconDetailMap.keySet().forEach(entity -> iconMap.put(entity, createIcon(entity)));
    }
    
    private JComponent createIcon(Entity entity){
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

    public JComponent getIcon(Entity entity){
      	return iconMap.get(entity);
    }

}
