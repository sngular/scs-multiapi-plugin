package com.sngular.api.generator.plugin.common.model;

import java.util.List;
import java.util.Map;

public interface IOperationObject {

  List<String> getOperationIds();

  Map<String, String> getFormats();

  String getIds();

  String getApiPackage();

  String getModelPackage();

  String getModelNameSuffix();

  String getModelNamePostfix();

  boolean isUseLombokModelAnnotation();

  String getDateTimeFormat();

  String getDateFormat();

  TimeType getUseTimeType();
  
}
