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

  protected String getIds() {
    return ids;
  }

  public void setIds(String ids) {
    this.ids = ids;

    this.operationIds = List.of(ids.replace(" ", "").split(","));
  }

  public String getModelPackage() {
    return modelPackage;
  }

  protected void setModelPackage(final String modelPackage) {
    this.modelPackage = modelPackage;
  }

  public String getTargetPackage() {
    return targetPackage;
  }

  protected void setTargetPackage(final String targetPackage) {
    this.targetPackage = targetPackage;
  }

  public List<String> getOperationIds() {
    return operationIds;
  }

  protected void setOperationIds(final List<String> operationIds) {
    this.operationIds = operationIds;
  }

  public String getEntitiesPostfix() {
    return entitiesPostfix;
  }

  protected void setEntitiesPostfix(final String entitiesPostfix) {
    this.entitiesPostfix = entitiesPostfix;
  }

  public String getClassNamePostfix() {
    return classNamePostfix;
  }

  protected void setClassNamePostfix(final String classNamePostfix) {
    this.classNamePostfix = classNamePostfix;
  }
}
