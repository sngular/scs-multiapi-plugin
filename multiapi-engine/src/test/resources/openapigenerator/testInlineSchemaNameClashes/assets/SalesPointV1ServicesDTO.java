package com.sngular.multifileplugin.inlineschemanameclashes.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = SalesPointV1ServicesDTO.SalesPointV1ServicesDTOBuilder.class)
public class SalesPointV1ServicesDTO {

  @JsonProperty(value ="serviceType")
  private String serviceType;
  @JsonProperty(value ="serviceId")
  private Long serviceId;

  private SalesPointV1ServicesDTO(SalesPointV1ServicesDTOBuilder builder) {
    this.serviceType = builder.serviceType;
    this.serviceId = builder.serviceId;

  }

  public static SalesPointV1ServicesDTO.SalesPointV1ServicesDTOBuilder builder() {
    return new SalesPointV1ServicesDTO.SalesPointV1ServicesDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SalesPointV1ServicesDTOBuilder {

    private String serviceType;
    private Long serviceId;

    public SalesPointV1ServicesDTO.SalesPointV1ServicesDTOBuilder serviceType(String serviceType) {
      this.serviceType = serviceType;
      return this;
    }

    public SalesPointV1ServicesDTO.SalesPointV1ServicesDTOBuilder serviceId(Long serviceId) {
      this.serviceId = serviceId;
      return this;
    }

    public SalesPointV1ServicesDTO build() {
      SalesPointV1ServicesDTO salesPointV1ServicesDTO = new SalesPointV1ServicesDTO(this);
      return salesPointV1ServicesDTO;
    }
  }

  @Schema(name = "serviceType", required = false)
  public String getServiceType() {
    return serviceType;
  }
  public void setServiceType(String serviceType) {
    this.serviceType = serviceType;
  }

  @Schema(name = "serviceId", required = false)
  public Long getServiceId() {
    return serviceId;
  }
  public void setServiceId(Long serviceId) {
    this.serviceId = serviceId;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SalesPointV1ServicesDTO salesPointV1ServicesDTO = (SalesPointV1ServicesDTO) o;
    return Objects.equals(this.serviceType, salesPointV1ServicesDTO.serviceType) && Objects.equals(this.serviceId, salesPointV1ServicesDTO.serviceId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(serviceType, serviceId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("SalesPointV1ServicesDTO{");
    sb.append(" serviceType:").append(serviceType).append(",");
    sb.append(" serviceId:").append(serviceId);
    sb.append("}");
    return sb.toString();
  }


}
