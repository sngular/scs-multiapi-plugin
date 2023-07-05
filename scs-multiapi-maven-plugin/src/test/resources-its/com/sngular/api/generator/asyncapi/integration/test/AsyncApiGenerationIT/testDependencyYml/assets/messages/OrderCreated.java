package com.sngular.apigenerator.asyncapi.model.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = OrderCreated.OrderCreatedBuilder.class)
public class OrderCreated {

  @JsonProperty(value ="payload")
  private com.sngular.apigenerator.asyncapi.model.schemas.Order payload;

  private OrderCreated(com.sngular.apigenerator.asyncapi.model.schemas.Order payload) {
    this.payload = payload;

  }

  private OrderCreated(OrderCreatedBuilder builder) {
    this.payload = builder.payload;

  }

  public static OrderCreated.OrderCreatedBuilder builder() {
    return new OrderCreated.OrderCreatedBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderCreatedBuilder {

    private com.sngular.apigenerator.asyncapi.model.schemas.Order payload;

    public OrderCreated.OrderCreatedBuilder payload(com.sngular.apigenerator.asyncapi.model.schemas.Order payload) {
      this.payload = payload;
      return this;
    }

    public OrderCreated build() {
      OrderCreated orderCreated = new OrderCreated(this);
      return orderCreated;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public com.sngular.apigenerator.asyncapi.model.schemas.Order getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.apigenerator.asyncapi.model.schemas.Order payload) {
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
    OrderCreated orderCreated = (OrderCreated) o;
    return Objects.equals(this.payload, orderCreated.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderCreated {\n");
    sb.append(" payload: ").append(toIndentedString(payload)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }



}
