/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.security.SecurityScheme;
import net.coru.api.generator.plugin.openapi.model.AuthObject;
import net.coru.api.generator.plugin.openapi.model.AuthSchemaObject;
import net.coru.api.generator.plugin.openapi.model.OperationObject;
import net.coru.api.generator.plugin.openapi.model.PathObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;

public class MapperAuthUtil {

  private static final String API_KEY = "apiKey";

  private MapperAuthUtil() {}

  public static List<AuthSchemaObject> createAuthSchemaList(final OpenAPI openAPI) {
    final ArrayList<AuthSchemaObject> authList = new ArrayList<>();
    if (Objects.nonNull(openAPI.getComponents()) && MapUtils.isNotEmpty(openAPI.getComponents().getSecuritySchemes())) {
      openAPI.getComponents().getSecuritySchemes().forEach((key, value) -> {
        final var typeStr = value.getType().toString();
        final var isHttpBearer = "http".equalsIgnoreCase(typeStr) && "bearer".equalsIgnoreCase(value.getScheme());
        final var authSchema = AuthSchemaObject.builder()
                                               .type(isHttpBearer ? "HttpBearerAuth" : getModelTypeAuth(value)).name(key)
                                               .apiKeyParam(API_KEY.equalsIgnoreCase(typeStr) ? value.getName() : "")
                                               .apiKeyPlace(API_KEY.equalsIgnoreCase(typeStr) ? value.getIn().toString() : "")
                                               .bearerSchema(isHttpBearer ? value.getScheme() : "").build();
        authList.add(authSchema);
      });
    }
    return authList;
  }

  private static String getModelTypeAuth(final SecurityScheme securityScheme) {
    var type = securityScheme.getType().toString();
    if (securityScheme.getType().toString().equalsIgnoreCase(API_KEY)) {
      type = "ApiKeyAuth";
    } else if (securityScheme.getType().toString().equalsIgnoreCase("oauth2")) {
      type = "OAuth";
    } else if (securityScheme.getType().toString().equalsIgnoreCase("http")) {
      type = "HttpBasicAuth";
    }

    return type;
  }

  public static AuthObject getApiAuthObject(final List<AuthSchemaObject> authSchemas, final List<PathObject> pathObjects) {
    final var authList = getApiAuthNames(pathObjects);
    final var authApiList = new ArrayList<String>();
    if (null != authSchemas && !authSchemas.isEmpty() && !authList.isEmpty()) {
      authSchemas.forEach(authValue -> {
        if (authList.contains(authValue.getName()) && !authApiList.contains(authValue.getType())) {
          authApiList.add(authValue.getType());
        }
      });
    }
    return AuthObject.builder().securityRequirements(authApiList).build();
  }

  private static List<String> getApiAuthNames(final List<PathObject> pathObjects) {
    final var operationList = new ArrayList<OperationObject>();
    pathObjects.forEach(pathObject -> operationList.addAll(pathObject.getOperationObjects()));
    return addApiAuthNames(operationList);
  }

  private static List<String> addApiAuthNames(final List<OperationObject> operationList) {
    final var authList = new ArrayList<String>();

    operationList.forEach(operationObject -> {
      if (CollectionUtils.isNotEmpty(operationObject.getSecurities())) {
        operationObject.getSecurities().forEach(auth -> {
          if (!authList.contains(auth)) {
            authList.add(auth);
          }
        });
      }
    });

    return authList;
  }


}
