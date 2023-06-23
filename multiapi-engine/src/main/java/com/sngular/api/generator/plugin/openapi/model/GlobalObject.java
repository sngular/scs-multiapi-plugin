/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

  public static class GlobalObjectBuilder {

    private final List<String> serverUrl = new ArrayList<>();

    private final List<AuthSchemaObject> authSchemas = new ArrayList<>();

    private final List<String> authentications = new ArrayList<>();

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
  }

}
