package com.sngular.multifileplugin.inlineschemanameclashes.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.inlineschemanameclashes.model.exception.ModelClassException;
import com.sngular.multifileplugin.inlineschemanameclashes.model.customvalidator.NotNull;

@JsonDeserialize(builder = NewSalesPointServicesDTO.NewSalesPointServicesDTOBuilder.class)
public class NewSalesPointServicesDTO {

  @JsonProperty(value ="service_id")
  @NotNull
  private final Long service_id;

  private NewSalesPointServicesDTO(NewSalesPointServicesDTOBuilder builder) {
    this.service_id = builder.service_id;

    validateRequiredAttributes();
  }

  public static NewSalesPointServicesDTO.NewSalesPointServicesDTOBuilder builder() {
    return new NewSalesPointServicesDTO.NewSalesPointServicesDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class NewSalesPointServicesDTOBuilder {

    private Long service_id;

    public NewSalesPointServicesDTO.NewSalesPointServicesDTOBuilder service_id(Long service_id) {
      this.service_id = service_id;
      return this;
    }

    public NewSalesPointServicesDTO build() {
      NewSalesPointServicesDTO newSalesPointServicesDTO = new NewSalesPointServicesDTO(this);
      return newSalesPointServicesDTO;
    }
  }

  @Schema(name = "service_id", required = true)
  public Long getService_id() {
    return service_id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NewSalesPointServicesDTO newSalesPointServicesDTO = (NewSalesPointServicesDTO) o;
    return Objects.equals(this.service_id, newSalesPointServicesDTO.service_id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(service_id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("NewSalesPointServicesDTO{");
    sb.append(" service_id:").append(service_id);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.service_id)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("NewSalesPointServicesDTO");
    }
  }

}
