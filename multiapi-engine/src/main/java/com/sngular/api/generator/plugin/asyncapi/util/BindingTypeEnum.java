package com.sngular.api.generator.plugin.asyncapi.util;

import lombok.Getter;

@Getter
public enum BindingTypeEnum {
  NONBINDING("NONBINDING"),
  KAFKA("KAFKA");

  private final String value;

  BindingTypeEnum(final String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }
}
