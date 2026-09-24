package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class SequenceFieldDTO {

  @JsonProperty(value ="elements")
  private Integer elements;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  @Singular(value = "property", ignoreNullCollections = true)
  private List<FieldDTO> properties;

  @JsonProperty(value ="defaultValues")
  @Singular(value = "defaultValue", ignoreNullCollections = true)
  private List<Object> defaultValues;

  @JsonProperty(value ="seqEnum")
  private SeqEnum seqEnum;

  public enum SeqEnum {
    MONTH("MONTH"),
    YEAR("YEAR"),
    HOUR("HOUR"),
    MINUTE("MINUTE"),
    SECOND("SECOND"),
    DAY("DAY"),
    UNKNOWN("UNKNOWN");

    private String value;

    SeqEnum(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static SeqEnum fromValue(String value) {
      if (value == null) {
        return null;
      }
      for (SeqEnum constant : values()) {
        if (value.equals(constant.value)) {
          return constant;
        }
      }
      return UNKNOWN;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="initialValue")
  private String initialValue;

  @JsonProperty(value ="increment")
  private Integer increment;


  @Builder
  @Jacksonized
  private SequenceFieldDTO(Integer elements, String type, List<FieldDTO> properties, List<Object> defaultValues, SeqEnum seqEnum, String name, String initialValue, Integer increment) {
    this.elements = elements;
    this.type = type;
    this.properties = properties;
    this.defaultValues = defaultValues;
    this.seqEnum = seqEnum;
    this.name = name;
    this.initialValue = initialValue;
    this.increment = increment;

  }

}
