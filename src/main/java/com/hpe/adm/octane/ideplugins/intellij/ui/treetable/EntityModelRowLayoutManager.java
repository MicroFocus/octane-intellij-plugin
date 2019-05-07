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

package com.hpe.adm.octane.ideplugins.intellij.ui.treetable;

import org.jetbrains.annotations.NotNull;

import java.awt.*;

/**
 * Layout manager specifically created for {@link EntityModelRow}
 * leftComponent grows, rightComponent has fixed width
 * If leftComponent gets too small, rightComponent is no longer shown
 */
class EntityModelRowLayoutManager implements LayoutManager {

    private Component leftComponent;
    private Component rightComponent;

    EntityModelRowLayoutManager(@NotNull Component leftComponent, @NotNull Component rightComponent) {
        setLeftComponent(leftComponent);
        setRightComponent(rightComponent);
    }

    private void setLeftComponent(Component leftComponent) {
        this.leftComponent = leftComponent;
    }

    private void setRightComponent(Component rightComponent) {
        this.rightComponent = rightComponent;
    }

    @Override
    public void addLayoutComponent(String name, Component comp) {
        if ("left".equals(name)) {
            setLeftComponent(comp);
        } else if ("right".equals(name)) {
            setRightComponent(comp);
        }
    }

    @Override
    public void removeLayoutComponent(Component comp) {
        leftComponent = comp == leftComponent ? null : leftComponent;
        rightComponent = comp == rightComponent ? null : rightComponent;
    }

    @Override
    public Dimension preferredLayoutSize(Container parent) {
        return leftComponent.getPreferredSize();
    }

    @Override
    public Dimension minimumLayoutSize(Container parent) {
        return leftComponent.getMinimumSize();
    }

    @Override
    public void layoutContainer(Container parent) {
        Insets insets = parent.getInsets();
        int top = insets.top;
        int left = insets.left;
        int height = parent.getHeight() - insets.bottom;
        int width = parent.getWidth() - insets.right;

        int leftWidth;
        int rightWidth;

        Dimension dimension = rightComponent.getPreferredSize();
        rightWidth = dimension.width;
        leftWidth = width - rightWidth;

        if (rightWidth > leftWidth) {
            rightWidth = 0;
            leftWidth = width;
        }

        rightComponent.setBounds(leftWidth, top, rightWidth, height);
        leftComponent.setBounds(left, top, leftWidth, height);
    }

}