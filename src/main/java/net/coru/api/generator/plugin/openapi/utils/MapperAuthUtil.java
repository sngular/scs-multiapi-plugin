/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import java.util.ArrayList;
import java.util.List;

import net.coru.api.generator.plugin.openapi.model.AuthObject;
import net.coru.api.generator.plugin.openapi.model.AuthSchemaObject;
import net.coru.api.generator.plugin.openapi.model.OperationObject;
import net.coru.api.generator.plugin.openapi.model.PathObject;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.security.SecurityScheme;

public class MapperAuthUtil {

  public static List<AuthSchemaObject> createAuthSchemaList(OpenAPI openAPI) {
    ArrayList<AuthSchemaObject> authList = new ArrayList<>();
    if (null != openAPI.getComponents().getSecuritySchemes()
        && !openAPI.getComponents().getSecuritySchemes().isEmpty()) {

      openAPI.getComponents().getSecuritySchemes().forEach((key, value) -> {
        var authSchema = AuthSchemaObject.builder()
                                         .type(value.getType().toString().equalsIgnoreCase("http")
                                               && value.getScheme().equalsIgnoreCase("bearer") ? "HttpBearerAuth" : getModelTypeAuth(value))
                                         .name(key)
                                         .apiKeyParam(value.getType().toString().equalsIgnoreCase("apiKey")
                                                        ? value.getName() : "")
                                         .apiKeyPlace(value.getType().toString().equalsIgnoreCase("apiKey")
                                                        ? value.getIn().toString() : "")
                                         .bearerSchema(value.getType().toString().equalsIgnoreCase("http")
                                                       && value.getScheme().equalsIgnoreCase("bearer") ? value.getScheme() : "")
                                         .build();

        authList.add(authSchema);
      });

    }

    return authList;
  }

  private static String getModelTypeAuth(SecurityScheme securityScheme) {
    var type = securityScheme.getType().toString();
    if (securityScheme.getType().toString().equalsIgnoreCase("apiKey")) {
      type = "ApiKeyAuth";
    } else if (securityScheme.getType().toString().equalsIgnoreCase("oauth2")) {
      type = "OAuth";
    } else if (securityScheme.getType().toString().equalsIgnoreCase("http")) {
      type = "HttpBasicAuth";
    }

    return type;
  }

  public static AuthObject getApiAuthObject(List<AuthSchemaObject> authSchemas, List<PathObject> pathObjects) {
    var authList = getApiAuthNames(pathObjects);
    var authApiList = new ArrayList<String>();
    if (null != authSchemas && !authSchemas.isEmpty() && !authList.isEmpty()) {
      authSchemas.forEach(authValue -> {
        if (authList.contains(authValue.getName()) && !authApiList.contains(authValue.getType())) {
          authApiList.add(authValue.getType());
        }
      });
    }
    return AuthObject.builder().securityRequirements(authApiList).build();
  }

  private static List<String> getApiAuthNames(List<PathObject> pathObjects) {
    var operationList = new ArrayList<OperationObject>();
    pathObjects.forEach(pathObject -> operationList.addAll(pathObject.getOperationObject()));
    return addApiAuthNames(operationList);
  }

  private static List<String> addApiAuthNames(List<OperationObject> operationList) {
    var authList = new ArrayList<String>();

    operationList.forEach(operationObject -> {
      if (null != operationObject.getSecurity() && !operationObject.getSecurity().isEmpty()) {
        operationObject.getSecurity().forEach(auth -> {
          if (!authList.contains(auth)) {
            authList.add(auth);
          }
        });
      }
    });

    return authList;
  }


}
