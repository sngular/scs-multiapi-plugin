package com.sngular.scsplugin.filegenerationissue.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ShippingDetailsDTO.ShippingDetailsDTOBuilder.class)
public class ShippingDetailsDTO {

  @JsonProperty(value ="address")
  private String address;

  private ShippingDetailsDTO(String address) {
    this.address = address;

  }

  private ShippingDetailsDTO(ShippingDetailsDTOBuilder builder) {
    this.address = builder.address;

  }

  public static ShippingDetailsDTO.ShippingDetailsDTOBuilder builder() {
    return new ShippingDetailsDTO.ShippingDetailsDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ShippingDetailsDTOBuilder {

    private String address;

    public ShippingDetailsDTO.ShippingDetailsDTOBuilder address(String address) {
      this.address = address;
      return this;
    }

    public ShippingDetailsDTO build() {
      ShippingDetailsDTO shippingDetailsDTO = new ShippingDetailsDTO(this);
      return shippingDetailsDTO;
    }
  }

  /**
  * Get address
  * @return address
  */
  @Schema(name = "address", required = false)
  public String getAddress() {
    return address;
  }
  public void setAddress(String address) {
    this.address = address;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ShippingDetailsDTO shippingDetailsDTO = (ShippingDetailsDTO) o;
    return Objects.equals(this.address, shippingDetailsDTO.address);
  }

  @Override
  public int hashCode() {
    return Objects.hash(address);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ShippingDetailsDTO{");
    sb.append(" address:").append(address).append(",");
    sb.append("}");
    return sb.toString();
  }




}
