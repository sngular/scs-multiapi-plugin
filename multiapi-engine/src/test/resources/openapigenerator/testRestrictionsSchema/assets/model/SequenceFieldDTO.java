package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class SequenceFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="elements")
  private Integer elements;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="seqEnum")
  private SeqEnum seqEnum;

  public enum SeqEnum {
    MONTH("MONTH"),
    YEAR("YEAR"),
    HOUR("HOUR"),
    MINUTE("MINUTE"),
    SECOND("SECOND"),
    DAY("DAY");

    private String value;

    SeqEnum(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="initialValue")
  private FieldDTO initialValue;

  @JsonProperty(value ="increment")
  private Integer increment;

  @JsonProperty(value ="property")
  private FieldDTO property;


  @Builder
  @Jacksonized
  private SequenceFieldDTO(Boolean mandatory, Integer elements, String type, SeqEnum seqEnum, String name, FieldDTO initialValue, Integer increment, FieldDTO property) {
    this.mandatory = mandatory;
    this.elements = elements;
    this.type = type;
    this.seqEnum = seqEnum;
    this.name = name;
    this.initialValue = initialValue;
    this.increment = increment;
    this.property = property;

  }

}
