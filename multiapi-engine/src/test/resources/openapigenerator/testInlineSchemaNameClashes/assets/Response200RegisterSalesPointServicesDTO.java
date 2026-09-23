package com.sngular.multifileplugin.inlineschemanameclashes.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Response200RegisterSalesPointServicesDTO.Response200RegisterSalesPointServicesDTOBuilder.class)
public class Response200RegisterSalesPointServicesDTO {

  @JsonProperty(value ="activated")
  private Boolean activated;

  private Response200RegisterSalesPointServicesDTO(Response200RegisterSalesPointServicesDTOBuilder builder) {
    this.activated = builder.activated;

  }

  public static Response200RegisterSalesPointServicesDTO.Response200RegisterSalesPointServicesDTOBuilder builder() {
    return new Response200RegisterSalesPointServicesDTO.Response200RegisterSalesPointServicesDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class Response200RegisterSalesPointServicesDTOBuilder {

    private Boolean activated;

    public Response200RegisterSalesPointServicesDTO.Response200RegisterSalesPointServicesDTOBuilder activated(Boolean activated) {
      this.activated = activated;
      return this;
    }

    public Response200RegisterSalesPointServicesDTO build() {
      Response200RegisterSalesPointServicesDTO response200RegisterSalesPointServicesDTO = new Response200RegisterSalesPointServicesDTO(this);
      return response200RegisterSalesPointServicesDTO;
    }
  }

  @Schema(name = "activated", required = false)
  public Boolean getActivated() {
    return activated;
  }
  public void setActivated(Boolean activated) {
    this.activated = activated;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Response200RegisterSalesPointServicesDTO response200RegisterSalesPointServicesDTO = (Response200RegisterSalesPointServicesDTO) o;
    return Objects.equals(this.activated, response200RegisterSalesPointServicesDTO.activated);
  }

  @Override
  public int hashCode() {
    return Objects.hash(activated);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Response200RegisterSalesPointServicesDTO{");
    sb.append(" activated:").append(activated);
    sb.append("}");
    return sb.toString();
  }


}
