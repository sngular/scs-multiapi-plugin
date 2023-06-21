/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.model;

import com.sngular.api.generator.plugin.asyncapi.util.BindingTypeEnum;
import lombok.Builder;
import lombok.Value;
import org.apache.commons.lang3.StringUtils;

@Value
public class MethodObject {

  String operationId;

  String classNamespace;

  String className;

  String type;

  String channelName;

  SchemaObject schemaObject;

  String keyClassName;

  String bindingType;

  public MethodObject(final String operationId, final String classNamespace, final String type, final String keyClassName, final String bindingType) {
    this(operationId, classNamespace, type, null, keyClassName, bindingType);
  }

  public MethodObject(final String operationId, final String classNamespace, final String type, final String channelName, final String keyClassName, final String bindingType) {
    this(operationId, classNamespace, type, channelName, null, keyClassName, bindingType);
  }

  @Builder(toBuilder = true)
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

}
