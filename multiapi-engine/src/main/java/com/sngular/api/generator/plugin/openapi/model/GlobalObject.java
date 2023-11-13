/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GlobalObject {

  private String url;

  private List<String> serverUrl;

  private List<AuthSchemaObject> authSchemas;

  private List<String> authentications;

  private Map<String, JsonNode> schemaMap;

  private Map<String, JsonNode> parameterMap;

  private Map<String, JsonNode> responseMap;

  private Map<String, JsonNode> requestBodyMap;

  public Optional<JsonNode> getSchemaNode(final String schemaName) {
    return Optional.ofNullable(schemaMap.get(schemaName));
  }

  public Optional<JsonNode> getResponseNode(final String schemaName) {
    return Optional.ofNullable(responseMap.get(schemaName));
  }

  public Optional<JsonNode> getParameterNode(final String schemaName) {
    return Optional.ofNullable(parameterMap.get(schemaName));
  }

  public Optional<JsonNode> getRequestBodyNode(final String schemaName) {
    return Optional.ofNullable(requestBodyMap.get(schemaName));
  }

  public static class GlobalObjectBuilder {

    private final List<String> serverUrl = new ArrayList<>();

    private final List<AuthSchemaObject> authSchemas = new ArrayList<>();

    private final List<String> authentications = new ArrayList<>();

    private final Map<String, JsonNode> schemaMap = new HashMap<>();

    private final Map<String, JsonNode> parameterMap = new HashMap<>();

    private final Map<String, JsonNode> responseMap = new HashMap<>();

    public final GlobalObjectBuilder serverUrl(final List<String> serverUrl) {
      this.serverUrl.addAll(serverUrl);
      return this;
    }

    public final GlobalObjectBuilder serverUrl(final String serverUrl) {
      this.serverUrl.add(serverUrl);
      return this;
    }

    public final GlobalObjectBuilder authSchemas(final List<AuthSchemaObject> authSchemas) {
      this.authSchemas.addAll(authSchemas);
      return this;
    }

    public final GlobalObjectBuilder authSchema(final AuthSchemaObject authSchema) {
      this.authSchemas.add(authSchema);
      return this;
    }

    public final GlobalObjectBuilder authentications(final List<String> authentications) {
      this.authentications.addAll(authentications);
      return this;
    }

    public final GlobalObjectBuilder authentication(final String authentication) {
      this.authentications.add(authentication);
      return this;
    }

    public final GlobalObjectBuilder schemaMap(final Map<String, JsonNode> schemaMap) {
      this.schemaMap.putAll(schemaMap);
      return this;
    }

    public final GlobalObjectBuilder schema(final String name, final JsonNode schema) {
      this.schemaMap.put(name, schema);
      return this;
    }

    public final GlobalObjectBuilder parameterMap(final Map<String, JsonNode> schemaMap) {
      this.parameterMap.putAll(schemaMap);
      return this;
    }

    public final GlobalObjectBuilder parameter(final String name, final JsonNode schema) {
      this.parameterMap.put(name, schema);
      return this;
    }

    public final GlobalObjectBuilder responseMap(final Map<String, JsonNode> schemaMap) {
      this.responseMap.putAll(schemaMap);
      return this;
    }

    public final GlobalObjectBuilder response(final String name, final JsonNode schema) {
      this.responseMap.put(name, schema);
      return this;
    }
  }
}
