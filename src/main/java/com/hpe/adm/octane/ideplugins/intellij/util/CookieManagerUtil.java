/*******************************************************************************
 * Copyright 2017-2023 Open Text.
 *
 * The only warranties for products and services of Open Text and
 * its affiliates and licensors (“Open Text”) are as may be set forth
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

package com.hpe.adm.octane.ideplugins.intellij.util;

import com.intellij.openapi.diagnostic.Logger;

import java.net.*;
import java.util.List;
import java.util.stream.Collectors;

public class CookieManagerUtil {

    private static final Logger logger = Logger.getInstance(CookieManagerUtil.class.getName());
    private static final CookieManager defaultCookieManager = new CookieManager();

    public static synchronized boolean clearCookies(String octaneServerURLString) {

        URI octaneServerURI;

        try {
            octaneServerURI = new URI(octaneServerURLString);
        } catch (URISyntaxException e) {
            logger.error(e);
            return false;
        }

        CookieManager currentCookieManager;

        if(CookieHandler.getDefault() != null && !(CookieHandler.getDefault() instanceof  CookieManager)) {

            // something else is messing with the cookie handler, give up to not break anything
            return false;
        } else if (CookieHandler.getDefault() != null) {

            currentCookieManager = (CookieManager) CookieHandler.getDefault();
        } else {

            CookieHandler.setDefault(defaultCookieManager);
            currentCookieManager = defaultCookieManager;
        }

        if(currentCookieManager.getCookieStore() == null || currentCookieManager.getCookieStore().getCookies() == null) {
            return true; // nothing to remove
        }

        CookieStore currentCookieStore = currentCookieManager.getCookieStore();

        List<HttpCookie> serverCookies =
                currentCookieStore
                .getCookies()
                .stream()
                .filter(httpCookie -> octaneServerURLString.contains(httpCookie.getDomain())) // yes, we know this is not exact science
                .collect(Collectors.toList());

        serverCookies.forEach(httpCookie -> currentCookieManager.getCookieStore().remove(octaneServerURI, httpCookie));

        return
            currentCookieStore
                .getCookies()
                .stream()
                .noneMatch(httpCookie -> octaneServerURLString.contains(httpCookie.getDomain()));

    }

}