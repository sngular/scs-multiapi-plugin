package com.sngular.api.generator.plugin.asyncapi.util;

public enum BindingTypeEnum {
  NONBINDING("NONBINDING"),
  KAFKA("KAFKA");

  private final String value;

  BindingTypeEnum(String value) {this.value = value;}

  public String getValue() {return value;}

  @Override
  public String toString() {return String.valueOf(value);}
}
