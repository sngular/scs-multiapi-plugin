package net.coru.multifileplugin.testclientpackage.client.auth;

import org.springframework.http.HttpHeaders;
import org.springframework.util.MultiValueMap;

public interface Authentication {

    public void applyToParams(MultiValueMap< String, String> queryParams, HttpHeaders headerParams, MultiValueMap< String, String> cookieParams);
}