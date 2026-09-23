package com.sngular.multifileplugin.externalcomponentschemarefs.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.externalcomponentschemarefs.model.exception.ModelClassException;
import com.sngular.multifileplugin.externalcomponentschemarefs.model.customvalidator.NotNull;

@JsonDeserialize(builder = SalesPointBaseDTO.SalesPointBaseDTOBuilder.class)
public class SalesPointBaseDTO {

  @JsonProperty(value ="name")
  @NotNull
  private final String name;

  private SalesPointBaseDTO(SalesPointBaseDTOBuilder builder) {
    this.name = builder.name;

    validateRequiredAttributes();
  }

  public static SalesPointBaseDTO.SalesPointBaseDTOBuilder builder() {
    return new SalesPointBaseDTO.SalesPointBaseDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SalesPointBaseDTOBuilder {

    private String name;

    public SalesPointBaseDTO.SalesPointBaseDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public SalesPointBaseDTO build() {
      SalesPointBaseDTO salesPointBaseDTO = new SalesPointBaseDTO(this);
      return salesPointBaseDTO;
    }
  }

  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SalesPointBaseDTO salesPointBaseDTO = (SalesPointBaseDTO) o;
    return Objects.equals(this.name, salesPointBaseDTO.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("SalesPointBaseDTO{");
    sb.append(" name:").append(name);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("SalesPointBaseDTO");
    }
  }

}
