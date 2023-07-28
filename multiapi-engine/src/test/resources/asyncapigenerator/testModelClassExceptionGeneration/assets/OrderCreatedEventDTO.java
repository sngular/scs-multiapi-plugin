package com.sngular.scsplugin.modelclass.model.event.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = OrderCreatedEventDTO.OrderCreatedEventDTOBuilder.class)
public class OrderCreatedEventDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.modelclass.model.event.schemas.OrderDTO payload;

  private OrderCreatedEventDTO(com.sngular.scsplugin.modelclass.model.event.schemas.OrderDTO payload) {
    this.payload = payload;

  }

  private OrderCreatedEventDTO(OrderCreatedEventDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static OrderCreatedEventDTO.OrderCreatedEventDTOBuilder builder() {
    return new OrderCreatedEventDTO.OrderCreatedEventDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderCreatedEventDTOBuilder {

    private com.sngular.scsplugin.modelclass.model.event.schemas.OrderDTO payload;

    public OrderCreatedEventDTO.OrderCreatedEventDTOBuilder payload(com.sngular.scsplugin.modelclass.model.event.schemas.OrderDTO payload) {
      this.payload = payload;
      return this;
    }

    public OrderCreatedEventDTO build() {
      OrderCreatedEventDTO orderCreatedEventDTO = new OrderCreatedEventDTO(this);
      return orderCreatedEventDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public com.sngular.scsplugin.modelclass.model.event.schemas.OrderDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.modelclass.model.event.schemas.OrderDTO payload) {
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
    OrderCreatedEventDTO orderCreatedEventDTO = (OrderCreatedEventDTO) o;
    return Objects.equals(this.payload, orderCreatedEventDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderCreatedEventDTO{");
    sb.append(" payload:").append(payload);
    sb.append("}");
    return sb.toString();
  }




}
