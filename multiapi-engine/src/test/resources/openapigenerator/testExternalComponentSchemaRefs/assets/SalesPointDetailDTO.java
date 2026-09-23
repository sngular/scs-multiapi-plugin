package com.sngular.multifileplugin.externalcomponentschemarefs.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.externalcomponentschemarefs.model.exception.ModelClassException;
import com.sngular.multifileplugin.externalcomponentschemarefs.model.customvalidator.NotNull;

@JsonDeserialize(builder = SalesPointDetailDTO.SalesPointDetailDTOBuilder.class)
public class SalesPointDetailDTO {

  @JsonProperty(value ="name")
  @NotNull
  private final String name;
  @JsonProperty(value ="sales_point_id")
  private Long sales_point_id;

  private SalesPointDetailDTO(SalesPointDetailDTOBuilder builder) {
    this.name = builder.name;
    this.sales_point_id = builder.sales_point_id;

    validateRequiredAttributes();
  }

  public static SalesPointDetailDTO.SalesPointDetailDTOBuilder builder() {
    return new SalesPointDetailDTO.SalesPointDetailDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SalesPointDetailDTOBuilder {

    private String name;
    private Long sales_point_id;

    public SalesPointDetailDTO.SalesPointDetailDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public SalesPointDetailDTO.SalesPointDetailDTOBuilder sales_point_id(Long sales_point_id) {
      this.sales_point_id = sales_point_id;
      return this;
    }

    public SalesPointDetailDTO build() {
      SalesPointDetailDTO salesPointDetailDTO = new SalesPointDetailDTO(this);
      return salesPointDetailDTO;
    }
  }

  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  @Schema(name = "sales_point_id", required = false)
  public Long getSales_point_id() {
    return sales_point_id;
  }
  public void setSales_point_id(Long sales_point_id) {
    this.sales_point_id = sales_point_id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SalesPointDetailDTO salesPointDetailDTO = (SalesPointDetailDTO) o;
    return Objects.equals(this.name, salesPointDetailDTO.name) && Objects.equals(this.sales_point_id, salesPointDetailDTO.sales_point_id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, sales_point_id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("SalesPointDetailDTO{");
    sb.append(" name:").append(name).append(",");
    sb.append(" sales_point_id:").append(sales_point_id);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("SalesPointDetailDTO");
    }
  }

}
