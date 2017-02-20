package com.hpe.adm.octane.services.connection;

import com.hpe.adm.nga.sdk.network.OctaneHttpClient;


public interface HttpClientProvider {
    OctaneHttpClient geOctaneHttpClient();
}
