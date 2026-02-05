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

package com.hpe.adm.octane.ideplugins.intellij.ui.detail.entityfields;

import org.jetbrains.annotations.NotNull;

import java.awt.*;

/**
 * Layout manager that spaces it's children into equally divided columns
 */
class EqualColumnsLayoutManager implements LayoutManager {

    private Component leftComponent;
    private Component rightComponent;

    EqualColumnsLayoutManager(@NotNull Component leftComponent, @NotNull Component rightComponent) {
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

        int prefHeight = (int) leftComponent.getPreferredSize().getHeight();
        //int drawHeight = height > prefHeight ? height : prefHeight;
        int drawHeight = 900;

        leftComponent.setBounds(left, top, width/2, drawHeight);
        rightComponent.setBounds(width/2, top, width, drawHeight);
        System.out.println(leftComponent.getBounds());
        System.out.println(rightComponent.getBounds());
    }

}