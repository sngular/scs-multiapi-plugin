/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

  private List<AuthSchemaObject> authSchemas = new ArrayList<>();

  private List<String> authentications = new ArrayList<>();

  private Map<String, String> componentsTypeMap = new HashMap<>();

  public static class GlobalObjectBuilder {

    private List<String> serverUrl = new ArrayList<>();

    private List<AuthSchemaObject> authSchemas = new ArrayList<>();

    private List<String> authentications = new ArrayList<>();

    private Map<String, String> componentsTypeMap = new HashMap<>();

    public GlobalObjectBuilder serverUrl(final List<String> serverUrl) {
      this.serverUrl.addAll(serverUrl);
      return this;
    }

    public GlobalObjectBuilder serverUrl(final String serverUrl) {
      this.serverUrl.add(serverUrl);
      return this;
    }

    public GlobalObjectBuilder authSchemas(final List<AuthSchemaObject> authSchemas) {
      this.authSchemas.addAll(authSchemas);
      return this;
    }

    public GlobalObjectBuilder authSchema(final AuthSchemaObject authSchema) {
      this.authSchemas.add(authSchema);
      return this;
    }

    public GlobalObjectBuilder authentications(final List<String> authentications) {
      this.authentications.addAll(authentications);
      return this;
    }

    public GlobalObjectBuilder authentication(final String authentication) {
      this.authentications.add(authentication);
      return this;
    }

    public GlobalObjectBuilder componentsTypeMap(final Map<String, String> componentsTypeMap) {
      this.componentsTypeMap.putAll(componentsTypeMap);
      return this;
    }

    public GlobalObjectBuilder componentTypeMap(final String key, final String componentsTypeMap) {
      this.componentsTypeMap.put(key, componentsTypeMap);
      return this;
    }
  }

}
