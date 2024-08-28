package com.sngular.multifileplugin.testissuefaker.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Value;
import lombok.Singular;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Value
public class SchemaDTO {

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  @Singular("property")
  private List<FieldDTO> properties;

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  private String id;

  @JsonProperty(value ="definitions")
  @Singular("definition")
  private List<FieldDTO> definitions;

  @JsonProperty(value ="subjectName")
  @NonNull
  private String subjectName;

  @JsonProperty(value ="requiredFields")
  @Singular("requiredField")
  private List<String> requiredFields;

  @JsonProperty(value ="original")
  private Boolean original;


  @Builder
  @Jacksonized
  private SchemaDTO(String type, List<FieldDTO> properties, @NonNull String name, String id, List<FieldDTO> definitions, @NonNull String subjectName, List<String> requiredFields, Boolean original) {
    this.type = type;
    this.properties = properties;
    this.name = name;
    this.id = id;
    this.definitions = definitions;
    this.subjectName = subjectName;
    this.requiredFields = requiredFields;
    this.original = original;

  }

}
