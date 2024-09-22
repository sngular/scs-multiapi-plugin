/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;

import com.fasterxml.jackson.databind.JsonNode;
import com.sngular.api.generator.plugin.common.tools.ApiTool;
import com.sngular.api.generator.plugin.common.tools.MapperUtil;
import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;
import com.sngular.api.generator.plugin.openapi.model.AuthObject;
import com.sngular.api.generator.plugin.openapi.model.AuthSchemaObject;
import com.sngular.api.generator.plugin.openapi.model.OperationObject;
import com.sngular.api.generator.plugin.openapi.model.PathObject;
import org.apache.commons.collections4.CollectionUtils;

public class MapperAuthUtil {

  private static final String API_KEY = "apiKey";

  private MapperAuthUtil() {}

  public static List<AuthSchemaObject> createAuthSchemaList(final JsonNode openAPI) {
    final ArrayList<AuthSchemaObject> authList = new ArrayList<>();
    for (Entry<String, JsonNode> entry : ApiTool.getComponentSecuritySchemes(openAPI).entrySet()) {
      final String key = entry.getKey();
      final JsonNode value = entry.getValue();
      final var typeStr = ApiTool.getType(value);
      final var isHttpBearer = "http".equalsIgnoreCase(typeStr) && "bearer".equalsIgnoreCase(ApiTool.getNodeAsString(value, "scheme"));
      final var authSchema = AuthSchemaObject
          .builder()
          .name(StringCaseUtils.toCamelCase(MapperUtil.getKey(key)))
          .type(isHttpBearer ? "HttpBearerAuth" : getModelTypeAuth(value))
          .apiKeyParam(API_KEY.equalsIgnoreCase(typeStr) ? ApiTool.getName(value) : "")
          .apiKeyPlace(API_KEY.equalsIgnoreCase(typeStr) ? ApiTool.getNodeAsString(value, "in") : "")
          .bearerSchema(isHttpBearer ? ApiTool.getNodeAsString(value, "scheme") : "")
          .build();
      authList.add(authSchema);
    }
    return authList;
  }

  private static String getModelTypeAuth(final JsonNode securityScheme) {
    var type = ApiTool.getType(securityScheme);
    if (API_KEY.equalsIgnoreCase(type)) {
      type = "ApiKeyAuth";
    } else if ("oauth2".equalsIgnoreCase(type)) {
      type = "OAuth";
    } else if ("http".equalsIgnoreCase(type)) {
      type = "HttpBasicAuth";
    }

    return type;
  }

  public static AuthObject getApiAuthObject(final List<AuthSchemaObject> authSchemas, final List<PathObject> pathObjects) {
    final var authList = getApiAuthNames(pathObjects);
    final var authApiList = new HashSet<String>();
    if (CollectionUtils.isNotEmpty(authSchemas) && !authList.isEmpty()) {
      authSchemas.forEach(authValue -> {
        if (authList.contains(authValue.getName())) {
          authApiList.add(authValue.getType());
        }
      });
    }
    return AuthObject.builder().securityRequirements(new ArrayList<>(authApiList)).build();
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
