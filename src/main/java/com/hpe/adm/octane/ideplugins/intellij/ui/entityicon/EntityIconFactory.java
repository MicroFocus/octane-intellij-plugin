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
import com.hpe.adm.octane.ideplugins.services.connection.ConnectionSettingsProvider;
import com.hpe.adm.octane.ideplugins.services.filtering.Entity;
import com.intellij.util.ImageLoader;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/*
JBColor might not be backwards compatible with prev intellij versions
 */
@SuppressWarnings({"UseJBColor", "InspectionUsingGrayColors"})
@Singleton
public class EntityIconFactory {

    class CacheKey {
        int size;
        int fontSize;
        Entity type;
        boolean isActive;

        public CacheKey(int size, int fontSize, Entity type, boolean isActive) {
            this.size = size;
            this.fontSize = fontSize;
            this.type = type;
            this.isActive = isActive;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            CacheKey cacheKey = (CacheKey) o;
            return size == cacheKey.size &&
                    fontSize == cacheKey.fontSize &&
                    isActive == cacheKey.isActive &&
                    type == cacheKey.type;
        }

        @Override
        public int hashCode() {
            return Objects.hash(size, fontSize, type, isActive);
        }
    }

    @Inject
    private EntityLabelService entityLabelService;

    private static final Color fontColor = new Color(255, 255, 255);
    private static final Color unmappedEntityIconColor = new Color(0, 0, 0);
    private static final Image activeImg = ImageLoader.loadFromResource(Constants.IMG_ACTIVE_ITEM, EntityIconFactory.class);

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
        entityColorMap.put(Entity.BDD_SCENARIO, new Color(117,218, 77));
    }

    //cache the icons based on size, font and entity
    private Map<CacheKey, Image> imageCache = new HashMap<>();

    @Inject
    public EntityIconFactory(EntityLabelService entityLabelService, ConnectionSettingsProvider connectionSettingsProvider) {
        this.entityLabelService = entityLabelService;
        connectionSettingsProvider.addChangeHandler(() -> imageCache.clear());
    }

    /**
     * Use param entity null to get a disabled icons
     *
     * @param entity
     * @param iconSize
     * @param fontSize
     * @return
     */
    private Image createIconAsImage(Entity entity, int iconSize, int fontSize, boolean isActive) {

        Color iconColor = entityColorMap.getOrDefault(entity, unmappedEntityIconColor);
        String iconText = entity != null ? entityLabelService.getEntityInitials(entity) : "";

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

        if (isActive) {
            int activeImage_x = 60 * iconSize / 100;
            int ypercent = 60 * iconSize / 100;
            int activeImageSize = iconSize - activeImage_x;
            bg.drawImage(activeImg, activeImage_x, ypercent, activeImageSize, activeImageSize, null);
        }

        return image;
    }

    public Image getIconAsImage(Entity entity, int iconSize, int fontSize) {
        return createIconAsImage(entity, iconSize, fontSize, false);
    }

    public Image getIconAsImage(Entity entity, int iconSize, int fontSize, boolean isActive) {

        CacheKey key = new CacheKey(iconSize, fontSize, entity, isActive);

        if (!imageCache.containsKey(key)) {
            imageCache.put(key, createIconAsImage(entity, iconSize, fontSize, isActive));
        }

        return imageCache.get(key);
    }

}