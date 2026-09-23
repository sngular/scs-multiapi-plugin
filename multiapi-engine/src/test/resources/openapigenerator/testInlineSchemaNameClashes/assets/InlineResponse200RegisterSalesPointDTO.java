package com.sngular.multifileplugin.inlineschemanameclashes.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = InlineResponse200RegisterSalesPointDTO.InlineResponse200RegisterSalesPointDTOBuilder.class)
public class InlineResponse200RegisterSalesPointDTO {

  @JsonProperty(value ="services")
  private List<Response200RegisterSalesPointServicesDTO> services;

  private InlineResponse200RegisterSalesPointDTO(InlineResponse200RegisterSalesPointDTOBuilder builder) {
    this.services = builder.services;

  }

  public static InlineResponse200RegisterSalesPointDTO.InlineResponse200RegisterSalesPointDTOBuilder builder() {
    return new InlineResponse200RegisterSalesPointDTO.InlineResponse200RegisterSalesPointDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class InlineResponse200RegisterSalesPointDTOBuilder {

    private List<Response200RegisterSalesPointServicesDTO> services = new ArrayList<Response200RegisterSalesPointServicesDTO>();

    public InlineResponse200RegisterSalesPointDTO.InlineResponse200RegisterSalesPointDTOBuilder services(List<Response200RegisterSalesPointServicesDTO> services) {
      if (!services.isEmpty()) {
        this.services.addAll(services);
      }
      return this;
    }

    public InlineResponse200RegisterSalesPointDTO.InlineResponse200RegisterSalesPointDTOBuilder service(Response200RegisterSalesPointServicesDTO service) {
      if (Objects.nonNull(service)) {
        this.services.add(service);
      }
      return this;
    }

    public InlineResponse200RegisterSalesPointDTO build() {
      InlineResponse200RegisterSalesPointDTO inlineResponse200RegisterSalesPointDTO = new InlineResponse200RegisterSalesPointDTO(this);
      return inlineResponse200RegisterSalesPointDTO;
    }
  }

  @Schema(name = "services", required = false)
  public List<Response200RegisterSalesPointServicesDTO> getServices() {
    return services;
  }
  public void setServices(List<Response200RegisterSalesPointServicesDTO> services) {
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
    InlineResponse200RegisterSalesPointDTO inlineResponse200RegisterSalesPointDTO = (InlineResponse200RegisterSalesPointDTO) o;
    return Objects.equals(this.services, inlineResponse200RegisterSalesPointDTO.services);
  }

  @Override
  public int hashCode() {
    return Objects.hash(services);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("InlineResponse200RegisterSalesPointDTO{");
    sb.append(" services:").append(services);
    sb.append("}");
    return sb.toString();
  }


}
