/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.parameter;

import java.util.List;

public class OperationParameterObject {

  private String ids;

  private String targetPackage;

  private String modelPackage;

  private String entitiesPostfix;

  private String classNamePostfix;

  private List<String> operationIds;

  protected final String getIds() {
    return ids;
  }

  public final void setIds(final String ids) {
    this.ids = ids;

    this.operationIds = List.of(ids.replace(" ", "").split(","));
  }

  public final String getModelPackage() {
    return modelPackage;
  }

  protected final void setModelPackage(final String modelPackage) {
    this.modelPackage = modelPackage;
  }

  public final String getTargetPackage() {
    return targetPackage;
  }

  protected final void setTargetPackage(final String targetPackage) {
    this.targetPackage = targetPackage;
  }

  public final List<String> getOperationIds() {
    return operationIds;
  }

  protected final void setOperationIds(final List<String> operationIds) {
    this.operationIds = operationIds;
  }

  public final String getEntitiesPostfix() {
    return entitiesPostfix;
  }

  protected final void setEntitiesPostfix(final String entitiesPostfix) {
    this.entitiesPostfix = entitiesPostfix;
  }

  public final String getClassNamePostfix() {
    return classNamePostfix;
  }

  protected final void setClassNamePostfix(final String classNamePostfix) {
    this.classNamePostfix = classNamePostfix;
  }
}
