package com.sngular.api.generator.plugin.asyncapi.model;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import lombok.Data;

@Data
public class SchemaFieldObjectProperties {

  private String maximum = null;

  private String minimum = null;

  private Integer exclusiveMaximum = null;

  private Integer exclusiveMinimum = null;

  private Boolean uniqueItems = null;

  private Integer minItems = null;

  private Integer maxItems = null;

  private Integer minLength = null;

  private Integer maxLength = null;

  private String pattern = null;

  private String multipleOf = null;

  private Set<String> properties;

  public SchemaFieldObjectProperties() {
    properties = new HashSet<>();
  }

  public void setMultipleOf(final String multipleOf) {
    this.multipleOf = multipleOf;
    this.properties.add("MultipleOf");
  }

  public void setPattern(final String pattern) {
    this.pattern = pattern;
    if (Objects.nonNull(pattern)) {
      properties.add("Pattern");
    }
  }

  public void setMaxLength(final Integer maxLength) {
    this.maxLength = maxLength;
    if (Objects.nonNull(maxLength)) {
      properties.add("Size");
    }
  }

  public void setMinLength(final Integer minLength) {
    this.minLength = minLength;
    if (Objects.nonNull(minLength)) {
      properties.add("Size");
    }
  }

  public void setMaxItems(final Integer maxItems) {
    this.maxItems = maxItems;
    if (Objects.nonNull(maxItems)) {
      properties.add("MaxItems");
    }
  }

  public void setMinItems(final Integer minItems) {
    this.minItems = minItems;
    if (Objects.nonNull(minItems)) {
      properties.add("MinItems");
    }
  }

  public void setUniqueItems(final Boolean uniqueItems) {
    this.uniqueItems = uniqueItems;
    if (Objects.nonNull(uniqueItems)) {
      properties.add("UniqueItems");
    }
  }

  public void setMaximum(final String maximum) {
    this.maximum = maximum;
    if (Objects.nonNull(maximum)) {
      properties.add("Maximum");
    }
  }

  public void setMinimum(final String mininum) {
    this.minimum = mininum;
    if (Objects.nonNull(mininum)) {
      properties.add("Minimum");
    }
  }

}
