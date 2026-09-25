package com.sngular.multifileplugin.requestbodyrequired.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.requestbodyrequired.model.customvalidator.Size;

@JsonDeserialize(builder = OrderDTO.OrderDTOBuilder.class)
public class OrderDTO {

  @JsonProperty(value ="name")
  @Size(max =Integer.MAX_VALUE, min =1)
  private String name;

  private OrderDTO(OrderDTOBuilder builder) {
    this.name = builder.name;

  }

  public static OrderDTO.OrderDTOBuilder builder() {
    return new OrderDTO.OrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderDTOBuilder {

    private String name;

    public OrderDTO.OrderDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public OrderDTO build() {
      OrderDTO orderDTO = new OrderDTO(this);
      return orderDTO;
    }
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OrderDTO orderDTO = (OrderDTO) o;
    return Objects.equals(this.name, orderDTO.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderDTO{");
    sb.append(" name:").append(name);
    sb.append("}");
    return sb.toString();
  }


}
