/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PathObject {

  private String pathName;

  private GlobalObject globalObject;

  private List<OperationObject> operationObjects;

  public static final class PathObjectBuilder {

    private final List<OperationObject> operationObjects = new ArrayList<>();

    public PathObjectBuilder operationObjects(final List<OperationObject> operationObjects) {
      this.operationObjects.addAll(operationObjects);
      return this;
    }

    public PathObjectBuilder operationObject(final OperationObject operationObject) {
      this.operationObjects.add(operationObject);
      return this;
    }


  }

}
