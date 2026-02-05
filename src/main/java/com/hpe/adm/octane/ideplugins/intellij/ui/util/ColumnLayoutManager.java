/*******************************************************************************
 * Copyright 2017-2026 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors ("Open Text") are as may be set forth
 * in the express warranty statements accompanying such products and services.
 * Nothing herein should be construed as constituting an additional warranty.
 * Open Text shall not be liable for technical or editorial errors or
 * omissions contained herein. The information contained herein is subject
 * to change without notice.
 *
 * Except as specifically indicated otherwise, this document contains
 * confidential information and a valid license is required for possession,
 * use or copying. If this work is provided to the U.S. Government,
 * consistent with FAR 12.211 and 12.212, Commercial Computer Software,
 * Computer Software Documentation, and Technical Data for Commercial Items are
 * licensed to the U.S. Government under vendor's standard commercial license.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.hpe.adm.octane.ideplugins.intellij.ui.util;

import java.awt.*;

public class ColumnLayoutManager implements LayoutManager {

    @Override
    public void addLayoutComponent(String name, Component comp) {
    }

    @Override
    public void removeLayoutComponent(Component comp) {
    }

    @Override
    public Dimension preferredLayoutSize(Container parent) {
        Component[] components = parent.getComponents();

        Dimension resultDimension = new Dimension(0, 0);

        for(Component component : components) {
            Dimension childPrefSize = component.getPreferredSize();
            resultDimension.width += component.getPreferredSize().getWidth();
            resultDimension.height = resultDimension.height < childPrefSize.getHeight() ? childPrefSize.height : resultDimension.height;
        }

        resultDimension.width += parent.getInsets().left + parent.getInsets().right;
        resultDimension.height += parent.getInsets().top + parent.getInsets().bottom;
        return resultDimension;
    }

    @Override
    public Dimension minimumLayoutSize(Container parent) {
        Component[] components = parent.getComponents();

        Dimension resultDimension = new Dimension(0, 0);

        for(Component component : components) {
            resultDimension.width += component.getMinimumSize().getWidth();
            resultDimension.height += component.getMinimumSize().getHeight();
        }

        resultDimension.width += parent.getInsets().left + parent.getInsets().right;
        resultDimension.height += parent.getInsets().top + parent.getInsets().bottom;

        return resultDimension;
    }

    @Override
    public void layoutContainer(Container parent) {
        Component[] components = parent.getComponents();

        Insets insets = parent.getInsets();
        int top = insets.top;
        int left = insets.left;
        int parentHeight = parent.getHeight() - insets.bottom - insets.top;
        int parentWidth = parent.getWidth() - insets.right - insets.left;

        int currentX = left;

        int childWidth = parentWidth / components.length;

        for(Component component : components) {
            component.setBounds(currentX, top, childWidth, parentHeight);
            currentX += childWidth;
        }

    }

}