package com.sngular.multifileplugin.openapi32.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = FilterDTO.FilterDTOBuilder.class)
public class FilterDTO {

  @JsonProperty(value ="name")
  private String name;

  private FilterDTO(FilterDTOBuilder builder) {
    this.name = builder.name;

  }

  public static FilterDTO.FilterDTOBuilder builder() {
    return new FilterDTO.FilterDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class FilterDTOBuilder {

    private String name;

    public FilterDTO.FilterDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public FilterDTO build() {
      FilterDTO filterDTO = new FilterDTO(this);
      return filterDTO;
    }
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FilterDTO filterDTO = (FilterDTO) o;
    return Objects.equals(this.name, filterDTO.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("FilterDTO{");
    sb.append(" name:").append(name);
    sb.append("}");
    return sb.toString();
  }


}
