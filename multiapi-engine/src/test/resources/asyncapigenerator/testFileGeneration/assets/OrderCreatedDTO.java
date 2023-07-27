package com.sngular.scsplugin.filegeneration.model.event.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = OrderCreatedDTO.OrderCreatedDTOBuilder.class)
public class OrderCreatedDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.filegeneration.model.event.schemas.OrderDTO payload;

  private OrderCreatedDTO(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderDTO payload) {
    this.payload = payload;

  }

  private OrderCreatedDTO(OrderCreatedDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static OrderCreatedDTO.OrderCreatedDTOBuilder builder() {
    return new OrderCreatedDTO.OrderCreatedDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderCreatedDTOBuilder {

    private com.sngular.scsplugin.filegeneration.model.event.schemas.OrderDTO payload;

    public OrderCreatedDTO.OrderCreatedDTOBuilder payload(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderDTO payload) {
      this.payload = payload;
      return this;
    }

    public OrderCreatedDTO build() {
      OrderCreatedDTO orderCreatedDTO = new OrderCreatedDTO(this);
      return orderCreatedDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public com.sngular.scsplugin.filegeneration.model.event.schemas.OrderDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderDTO payload) {
    this.payload = payload;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OrderCreatedDTO orderCreatedDTO = (OrderCreatedDTO) o;
    return Objects.equals(this.payload, orderCreatedDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderCreatedDTO{");
    sb.append(" payload:").append(payload);
    sb.append("}");
    return sb.toString();
  }




}
