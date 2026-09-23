package com.sngular.multifileplugin.inlineschemanameclashes.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = SalesPointV1DTO.SalesPointV1DTOBuilder.class)
public class SalesPointV1DTO {

  @JsonProperty(value ="services")
  private List<SalesPointV1ServicesDTO> services;

  private SalesPointV1DTO(SalesPointV1DTOBuilder builder) {
    this.services = builder.services;

  }

  public static SalesPointV1DTO.SalesPointV1DTOBuilder builder() {
    return new SalesPointV1DTO.SalesPointV1DTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SalesPointV1DTOBuilder {

    private List<SalesPointV1ServicesDTO> services = new ArrayList<SalesPointV1ServicesDTO>();

    public SalesPointV1DTO.SalesPointV1DTOBuilder services(List<SalesPointV1ServicesDTO> services) {
      if (Objects.nonNull(services) && !services.isEmpty()) {
        this.services.addAll(services);
      }
      return this;
    }

    public SalesPointV1DTO.SalesPointV1DTOBuilder service(SalesPointV1ServicesDTO service) {
      if (Objects.nonNull(service)) {
        this.services.add(service);
      }
      return this;
    }

    public SalesPointV1DTO build() {
      SalesPointV1DTO salesPointV1DTO = new SalesPointV1DTO(this);
      return salesPointV1DTO;
    }
  }

  @Schema(name = "services", required = false)
  public List<SalesPointV1ServicesDTO> getServices() {
    return services;
  }
  public void setServices(List<SalesPointV1ServicesDTO> services) {
    this.services = services;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SalesPointV1DTO salesPointV1DTO = (SalesPointV1DTO) o;
    return Objects.equals(this.services, salesPointV1DTO.services);
  }

  @Override
  public int hashCode() {
    return Objects.hash(services);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("SalesPointV1DTO{");
    sb.append(" services:").append(services);
    sb.append("}");
    return sb.toString();
  }


}
