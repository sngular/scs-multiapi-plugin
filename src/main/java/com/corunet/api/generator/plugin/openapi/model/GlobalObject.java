/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.openapi.model;

import java.util.List;
import java.util.Map;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class GlobalObject {

  private String url;
  private String version;
  private String title;
  private String license;
  private List<String> serverUrl;
  private List<AuthSchemaObject> authSchemas;
  private List<String> authentications;
  private Map<String,String> componentsTypeMap;

}
