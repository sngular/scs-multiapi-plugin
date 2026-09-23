package com.sngular.multifileplugin.inlineschemanameclashes.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = NewSalesPointDTO.NewSalesPointDTOBuilder.class)
public class NewSalesPointDTO {

  @JsonProperty(value ="address")
  private AddressDTO address;
  @JsonProperty(value ="services")
  private List<NewSalesPointServicesDTO> services;

  private NewSalesPointDTO(NewSalesPointDTOBuilder builder) {
    this.address = builder.address;
    this.services = builder.services;

  }

  public static NewSalesPointDTO.NewSalesPointDTOBuilder builder() {
    return new NewSalesPointDTO.NewSalesPointDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class NewSalesPointDTOBuilder {

    private AddressDTO address;
    private List<NewSalesPointServicesDTO> services = new ArrayList<NewSalesPointServicesDTO>();

    public NewSalesPointDTO.NewSalesPointDTOBuilder address(AddressDTO address) {
      this.address = address;
      return this;
    }

    public NewSalesPointDTO.NewSalesPointDTOBuilder services(List<NewSalesPointServicesDTO> services) {
      if (!services.isEmpty()) {
        this.services.addAll(services);
      }
      return this;
    }

    public NewSalesPointDTO.NewSalesPointDTOBuilder service(NewSalesPointServicesDTO service) {
      if (Objects.nonNull(service)) {
        this.services.add(service);
      }
      return this;
    }

    public NewSalesPointDTO build() {
      NewSalesPointDTO newSalesPointDTO = new NewSalesPointDTO(this);
      return newSalesPointDTO;
    }
  }

  @Schema(name = "address", required = false)
  public AddressDTO getAddress() {
    return address;
  }
  public void setAddress(AddressDTO address) {
    this.address = address;
  }

  @Schema(name = "services", required = false)
  public List<NewSalesPointServicesDTO> getServices() {
    return services;
  }
  public void setServices(List<NewSalesPointServicesDTO> services) {
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
    NewSalesPointDTO newSalesPointDTO = (NewSalesPointDTO) o;
    return Objects.equals(this.address, newSalesPointDTO.address) && Objects.equals(this.services, newSalesPointDTO.services);
  }

  @Override
  public int hashCode() {
    return Objects.hash(address, services);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("NewSalesPointDTO{");
    sb.append(" address:").append(address).append(",");
    sb.append(" services:").append(services);
    sb.append("}");
    return sb.toString();
  }


}
