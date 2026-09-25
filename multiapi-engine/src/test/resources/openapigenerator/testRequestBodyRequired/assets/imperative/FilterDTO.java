package com.sngular.multifileplugin.requestbodyrequired.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = FilterDTO.FilterDTOBuilder.class)
public class FilterDTO {

  @JsonProperty(value ="status")
  private String status;

  private FilterDTO(FilterDTOBuilder builder) {
    this.status = builder.status;

  }

  public static FilterDTO.FilterDTOBuilder builder() {
    return new FilterDTO.FilterDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class FilterDTOBuilder {

    private String status;

    public FilterDTO.FilterDTOBuilder status(String status) {
      this.status = status;
      return this;
    }

    public FilterDTO build() {
      FilterDTO filterDTO = new FilterDTO(this);
      return filterDTO;
    }
  }

  @Schema(name = "status", required = false)
  public String getStatus() {
    return status;
  }
  public void setStatus(String status) {
    this.status = status;
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
    return Objects.equals(this.status, filterDTO.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("FilterDTO{");
    sb.append(" status:").append(status);
    sb.append("}");
    return sb.toString();
  }


}
