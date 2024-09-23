package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class SchemaDTO {

  @JsonProperty(value ="properties")
  @Singular("property")
  private List<FieldDTO> properties;

  @JsonProperty(value ="type")
  @NonNull
  private String type;

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  private String id;

  @JsonProperty(value ="subjectName")
  @NonNull
  private String subjectName;

  @JsonProperty(value ="original")
  private Boolean original;


  @Builder
  @Jacksonized
  private SchemaDTO(List<FieldDTO> properties, @NonNull String type, @NonNull String name, String id, @NonNull String subjectName, Boolean original) {
    this.properties = properties;
    this.type = type;
    this.name = name;
    this.id = id;
    this.subjectName = subjectName;
    this.original = original;

  }

}
