/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import com.sngular.api.generator.plugin.asyncapi.model.SchemaObject;
import com.sngular.api.generator.plugin.asyncapi.util.BindingTypeEnum;
import org.apache.commons.lang3.StringUtils;

public class MethodObject {

  private String operationId;

  private String classNamespace;

  private String className;

  private String type;

  private String channelName;

  private SchemaObject schemaObject;

  private String keyClassName;

  private String bindingType;

  public MethodObject(final String operationId, final String classNamespace, final String type, final String keyClassName, final String bindingType) {
    this(operationId, classNamespace, type, null, keyClassName, bindingType);
  }

  public MethodObject(final String operationId, final String classNamespace, final String type, final String channelName, final String keyClassName, final String bindingType) {
    this(operationId, classNamespace, type, channelName, null, keyClassName, bindingType);
  }

  public MethodObject(final String operationId, final String classNamespace, final String type, final String channelName, final SchemaObject schemaObject,
      final String keyClassName, final String bindingType) {
    this.operationId = operationId;
    this.classNamespace = classNamespace;
    final var splitNamespace = classNamespace.split("\\.");
    this.className = splitNamespace[splitNamespace.length - 1];
    this.type = type;
    this.channelName = channelName;
    this.schemaObject = schemaObject;
    this.keyClassName = keyClassName;
    this.bindingType = StringUtils.isEmpty(bindingType) ? BindingTypeEnum.NONBINDING.getValue() : bindingType;
  }

  protected final void setOperationId(final String operationId) {
    this.operationId = operationId;
  }

  protected final void setClassNamespace(final String classNamespace) {
    this.classNamespace = classNamespace;
  }

  protected final void setClassName(final String className) {
    this.className = className;
  }

  protected final void setType(final String type) {
    this.type = type;
  }

  protected final void setChannelName(final String channelName) {
    this.channelName = channelName;
  }

  protected final void setSchemaObject(final SchemaObject schemaObject) {
    this.schemaObject = schemaObject;
  }

  public final String getOperationId() {
    return operationId;
  }

  public final String getClassNamespace() {
    return classNamespace;
  }

  public final String getClassName() {
    return className;
  }

  public final String getType() {
    return type;
  }

  public final String getChannelName() {
    return channelName;
  }

  public final SchemaObject getSchemaObject() {
    return schemaObject;
  }

  public final String getBindingType() { return bindingType; }
}
