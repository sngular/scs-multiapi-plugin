/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.model;

import java.util.Objects;
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

  String keyClassNamespace;

  String bindingType;

  @Builder(toBuilder = true)
  public MethodObject(
      final String operationId, final String classNamespace, final String type, final String channelName, final SchemaObject schemaObject,
      final String keyClassNamespace, final String bindingType) {
    this.operationId = operationId;
    this.classNamespace = classNamespace.substring(0, classNamespace.lastIndexOf("."));
    this.className = classNamespace.substring(classNamespace.lastIndexOf(".") + 1);
    this.type = type;
    this.channelName = channelName;
    this.schemaObject = schemaObject;
    if (Objects.nonNull(keyClassNamespace)) {
      if (keyClassNamespace.contains(".")) {
        this.keyClassNamespace = keyClassNamespace.substring(0, keyClassNamespace.lastIndexOf("."));
        this.keyClassName = keyClassNamespace.substring(keyClassNamespace.lastIndexOf(".") + 1);
      } else {
        this.keyClassNamespace = null;
        this.keyClassName = keyClassNamespace;
      }
    } else {
      this.keyClassName = null;
      this.keyClassNamespace = null;
    }
    this.bindingType = StringUtils.isEmpty(bindingType) ? BindingTypeEnum.NONBINDING.getValue() : bindingType;
  }

}
