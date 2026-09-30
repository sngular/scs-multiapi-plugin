package com.sngular.multifileplugin.openapi32.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CriteriaDTO.CriteriaDTOBuilder.class)
public class CriteriaDTO {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="page")
  private Integer page;

  private CriteriaDTO(CriteriaDTOBuilder builder) {
    this.name = builder.name;
    this.page = builder.page;

  }

  public static CriteriaDTO.CriteriaDTOBuilder builder() {
    return new CriteriaDTO.CriteriaDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CriteriaDTOBuilder {

    private String name;
    private Integer page;

    public CriteriaDTO.CriteriaDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public CriteriaDTO.CriteriaDTOBuilder page(Integer page) {
      this.page = page;
      return this;
    }

    public CriteriaDTO build() {
      CriteriaDTO criteriaDTO = new CriteriaDTO(this);
      return criteriaDTO;
    }
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "page", required = false)
  public Integer getPage() {
    return page;
  }
  public void setPage(Integer page) {
    this.page = page;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CriteriaDTO criteriaDTO = (CriteriaDTO) o;
    return Objects.equals(this.name, criteriaDTO.name) && Objects.equals(this.page, criteriaDTO.page);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, page);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CriteriaDTO{");
    sb.append(" name:").append(name).append(",");
    sb.append(" page:").append(page);
    sb.append("}");
    return sb.toString();
  }


}
