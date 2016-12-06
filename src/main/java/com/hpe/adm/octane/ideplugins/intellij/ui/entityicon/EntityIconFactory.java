package com.hpe.adm.octane.ideplugins.intellij.ui.entityicon;

import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import org.jdesktop.swingx.JXLabel;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class EntityIconFactory {

    private static final int ICON_HEIGHT = 25;
    private static final int ICON_WIDTH = 25;
    private static final Color FONT_COLOR = new Color(255,255,255);

    //Detail for unmapped entity type
    private static final IconDetail unmapedEntityIconDetail = new IconDetail(new Color(0,0,0,0), "", true);

    //map to color and short text
    private static final Map<Entity, IconDetail> iconDetailMap = new HashMap<>();
    //Eager init
    private static final Map<Entity, JComponent> iconMap = new HashMap<>();
   
    //default values should reflect octane
    static {
        iconDetailMap.put(Entity.STORY, new IconDetail(new Color(218,199,120), "US"));
        iconDetailMap.put(Entity.DEFECT, new IconDetail(new Color(190,102,92), "D"));
        iconDetailMap.put(Entity.TASK, new IconDetail(new Color(137,204,174), "T"));
        iconDetailMap.put(Entity.MANUAL_TEST, new IconDetail(new Color(96,121,141), "MT"));
        iconDetailMap.put(Entity.GHERKIN_TEST, new IconDetail(new Color(120,196,192), "GT"));
    }

    static {
        iconDetailMap.keySet().forEach(entity -> iconMap.put(entity, createIcon(entity)));
    }
    
    private static JComponent createIcon(Entity entity){
    	IconDetail iconDetail = iconDetailMap.containsKey(entity) ? iconDetailMap.get(entity) : unmapedEntityIconDetail;
    	
        //Make the label
        Font defaultFont = new JXLabel().getFont();
        Font boldFont = new Font(defaultFont.getFontName(), Font.BOLD, defaultFont.getSize());

        JXLabel label = new JXLabel(iconDetail.getDisplayLabelText());
        
        label.setForeground(FONT_COLOR);
        label.setOpaque(iconDetail.isOpaque());
        label.setBackground(iconDetail.getColor());
        label.setPreferredSize(new Dimension(ICON_WIDTH, ICON_HEIGHT));
        label.setMinimumSize(new Dimension(ICON_WIDTH, ICON_HEIGHT));
        label.setMaximumSize(new Dimension(ICON_WIDTH, ICON_HEIGHT));
        label.setFont(boldFont);
        label.setHorizontalAlignment(SwingConstants.CENTER);
        label.setVerticalAlignment(SwingConstants.CENTER);

        return label;
    }

    public static JComponent getIcon(Entity entity){
      	return iconMap.get(entity);
    }

}
