package com.sngular.multifileplugin.testissuefaker.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class SchemaDTO {

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  private List<FieldDTO> properties = new ArrayList<FieldDTO>();

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  private String id;

  @JsonProperty(value ="definitions")
  private List<FieldDTO> definitions = new ArrayList<FieldDTO>();

  @JsonProperty(value ="subjectName")
  @NonNull
  private String subjectName;

  @JsonProperty(value ="requiredFields")
  private List<String> requiredFields = new ArrayList<String>();

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
