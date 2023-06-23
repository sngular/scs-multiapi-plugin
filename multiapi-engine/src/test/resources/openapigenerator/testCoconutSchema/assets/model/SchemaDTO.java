package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class SchemaDTO {

  @JsonProperty(value ="type")
  @NonNull
  private String type;

  @JsonProperty(value ="additionalProperties")
  private Map<String, FieldDTO> additionalProperties = new HashMap<String, FieldDTO>();

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private String id;

  @JsonProperty(value ="subjectName")
  @NonNull
  private String subjectName;

  @JsonProperty(value ="requiredFields")
  private List<String> requiredFields = new ArrayList<String>();


  @Builder
  @Jacksonized
  private SchemaDTO(@NonNull String type, Map<String, FieldDTO> additionalProperties, @NonNull String name, @NonNull String id, @NonNull String subjectName, List<String> requiredFields) {
    this.type = type;
    this.additionalProperties = additionalProperties;
    this.name = name;
    this.id = id;
    this.subjectName = subjectName;
    this.requiredFields = requiredFields;

  }

}
