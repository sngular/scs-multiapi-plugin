package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testClientPackageWebClientApiGeneration.assets;

import org.springframework.http.HttpHeaders;
import org.springframework.util.MultiValueMap;

public interface Authentication {

  public void applyToParams(MultiValueMap<String, String> queryParams, HttpHeaders headerParams, MultiValueMap<String, String> cookieParams);
}