package com.sngular.api.generator.plugin.asyncapi.model;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import lombok.Builder.Default;
import lombok.Data;

@Data
public class SchemaFieldObjectProperties {

  @Default
  private String maximum = null;

  @Default
  private String minimum = null;

  @Default
  private Boolean exclusiveMaximum = null;

  @Default
  private Boolean exclusiveMinimum = null;

  @Default
  private Boolean uniqueItems = null;

  @Default
  private Integer minItems = null;

  @Default
  private Integer maxItems = null;

  @Default
  private Integer minLength = null;

  @Default
  private Integer maxLength = null;

  @Default
  private String pattern = null;

  @Default
  private String multipleOf = null;

  private Set<String> properties;

  public SchemaFieldObjectProperties() {
    properties = new HashSet<>();
  }

  public void setMultipleOf(String multipleOf) {
    this.multipleOf = multipleOf;
    this.properties.add("MultipleOf");
  }

  public void setPattern(String pattern) {
    this.pattern = pattern;
    if (Objects.nonNull(pattern)) {
      properties.add("Pattern");
    }
  }

  public void setMaxLength(Integer maxLength) {
    this.maxLength = maxLength;
    if (Objects.nonNull(maxLength)) {
      properties.add("Size");
    }
  }

  public void setMinLength(Integer minLength) {
    this.minLength = minLength;
    if (Objects.nonNull(minLength)) {
      properties.add("Size");
    }
  }

  public void setMaxItems(Integer maxItems) {
    this.maxItems = maxItems;
    if (Objects.nonNull(maxItems)) {
      properties.add("MaxItems");
    }
  }

  public void setMinItems(Integer minItems) {
    this.minItems = minItems;
    if (Objects.nonNull(minItems)) {
      properties.add("MinItems");
    }
  }

  public void setUniqueItems(Boolean uniqueItems) {
    this.uniqueItems = uniqueItems;
    if (Objects.nonNull(uniqueItems)) {
      properties.add("UniqueItems");
    }
  }

  public void setMaximum(String maximum) {
    this.maximum = maximum;
    if (Objects.nonNull(maximum)) {
      properties.add("Maximum");
    }
  }

  public void setMinimum(String mininum) {
    this.minimum = mininum;
    if (Objects.nonNull(mininum)) {
      properties.add("Minimum");
    }
  }

}
