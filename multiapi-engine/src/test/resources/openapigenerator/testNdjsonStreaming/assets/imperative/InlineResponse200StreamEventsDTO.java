package com.sngular.multifileplugin.ndjsonstreaming.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = InlineResponse200StreamEventsDTO.InlineResponse200StreamEventsDTOBuilder.class)
public class InlineResponse200StreamEventsDTO {

  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="id")
  private String id;

  private InlineResponse200StreamEventsDTO(InlineResponse200StreamEventsDTOBuilder builder) {
    this.type = builder.type;
    this.id = builder.id;

  }

  public static InlineResponse200StreamEventsDTO.InlineResponse200StreamEventsDTOBuilder builder() {
    return new InlineResponse200StreamEventsDTO.InlineResponse200StreamEventsDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class InlineResponse200StreamEventsDTOBuilder {

    private String type;
    private String id;

    public InlineResponse200StreamEventsDTO.InlineResponse200StreamEventsDTOBuilder type(String type) {
      this.type = type;
      return this;
    }

    public InlineResponse200StreamEventsDTO.InlineResponse200StreamEventsDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public InlineResponse200StreamEventsDTO build() {
      InlineResponse200StreamEventsDTO inlineResponse200StreamEventsDTO = new InlineResponse200StreamEventsDTO(this);
      return inlineResponse200StreamEventsDTO;
    }
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    InlineResponse200StreamEventsDTO inlineResponse200StreamEventsDTO = (InlineResponse200StreamEventsDTO) o;
    return Objects.equals(this.type, inlineResponse200StreamEventsDTO.type) && Objects.equals(this.id, inlineResponse200StreamEventsDTO.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("InlineResponse200StreamEventsDTO{");
    sb.append(" type:").append(type).append(",");
    sb.append(" id:").append(id);
    sb.append("}");
    return sb.toString();
  }


}
