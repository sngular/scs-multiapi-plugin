/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.model

import com.sngular.api.generator.plugin.common.model.TypeConstants

class OperationParameter {

  String ids

  String apiPackage

  String modelPackage

  String modelNamePrefix

  String modelNameSuffix

  String classNamePostfix

  String dateFormat

  String dateTimeFormat

  TypeConstants.TimeType useTimeType

  boolean useLombokModelAnnotation

  boolean generateSpringwolfAnnotations

  boolean usePactAnnotation

  String getIds() {
    return ids
  }

  void setIds(final Object ids) {
    if (ids instanceof Collection) {
      this.ids = ids.join(",")
    } else {
      this.ids = ids?.toString()
    }
  }

  String getApiPackage() {
    return apiPackage
  }

  void setApiPackage(final String apiPackage) {
    this.apiPackage = apiPackage
  }

  String getModelPackage() {
    return modelPackage
  }

  void setModelPackage(final String modelPackage) {
    this.modelPackage = modelPackage
  }

  String getModelNamePrefix() {
    return modelNamePrefix
  }

  void setModelNamePrefix(final String modelNamePrefix) {
    this.modelNamePrefix = modelNamePrefix
  }

  String getModelNameSuffix() {
    return modelNameSuffix
  }

  void setModelNameSuffix(final String modelNameSuffix) {
    this.modelNameSuffix = modelNameSuffix
  }

  String getClassNamePostfix() {
    return classNamePostfix
  }

  void setClassNamePostfix(final String classNamePostfix) {
    this.classNamePostfix = classNamePostfix
  }

  String getDateFormat() {
    return dateFormat
  }

  void setDateFormat(final String dateFormat) {
    this.dateFormat = dateFormat
  }

  String getDateTimeFormat() {
    return dateTimeFormat
  }

  void setDateTimeFormat(final String dateTimeFormat) {
    this.dateTimeFormat = dateTimeFormat
  }

  TypeConstants.TimeType getUseTimeType() {
    return useTimeType
  }

  void setUseTimeType(final TypeConstants.TimeType useTimeType) {
    this.useTimeType = useTimeType
  }

  boolean getUseLombokModelAnnotation() {
    return useLombokModelAnnotation
  }

  void setUseLombokModelAnnotation(final boolean useLombokModelAnnotation) {
    this.useLombokModelAnnotation = useLombokModelAnnotation
  }

  boolean getGenerateSpringwolfAnnotations() {
    return generateSpringwolfAnnotations
  }

  void setGenerateSpringwolfAnnotations(final boolean generateSpringwolfAnnotations) {
    this.generateSpringwolfAnnotations = generateSpringwolfAnnotations
  }

  boolean getUsePactAnnotation() {
    return usePactAnnotation
  }

  void setUsePactAnnotation(final boolean usePactAnnotation) {
    this.usePactAnnotation = usePactAnnotation
  }

}

