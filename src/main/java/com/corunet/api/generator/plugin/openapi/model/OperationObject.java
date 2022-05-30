/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.openapi.model;

import java.util.List;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class OperationObject {

  private String operationType;
  private String summary;
  private String operationId;
  private List<String> tags;
  private List<ResponseObject> responseObjects;
  private List<RequestObject> requestObjects;
  private List<ParameterObject> parameterObjects;
  private List<String> produces;
  private List<String> consumes;
  private List<String> security;

}
