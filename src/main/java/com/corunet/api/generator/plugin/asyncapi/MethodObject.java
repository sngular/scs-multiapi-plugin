/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.asyncapi;

public class MethodObject {

  private String operationId;

  private String classNamespace;

  private String className;

  private String type;

  private String channelName;

  public MethodObject(String operationId, String classNamespace, String type) {
    this.operationId = operationId;
    this.classNamespace = classNamespace;
    var splitNamespace = classNamespace.split("\\.");
    this.className = splitNamespace[splitNamespace.length - 1];
    this.type = type;
    this.channelName = null;
  }

  public MethodObject(String operationId, String classNamespace, String type, String channelName) {
    this.operationId = operationId;
    this.classNamespace = classNamespace;
    var splitNamespace = classNamespace.split("\\.");
    this.className = splitNamespace[splitNamespace.length - 1];
    this.type = type;
    this.channelName = channelName;
  }

  protected void setOperationId(final String operationId) {
    this.operationId = operationId;
  }

  protected void setClassNamespace(final String classNamespace) {
    this.classNamespace = classNamespace;
  }

  protected void setClassName(final String className) {
    this.className = className;
  }

  protected void setType(final String type) {
    this.type = type;
  }

  protected void setChannelName(final String channelName) {
    this.channelName = channelName;
  }

  public String getOperationId() {
    return operationId;
  }

  public String getClassNamespace() {
    return classNamespace;
  }

  public String getClassName() {return className;}

  public String getType() {return type;}

  public String getChannelName() {return channelName;}
}
