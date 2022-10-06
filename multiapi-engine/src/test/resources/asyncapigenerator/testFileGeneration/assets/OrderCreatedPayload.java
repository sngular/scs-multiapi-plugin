package net.coru.scsplugin.business_model.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.business_model.model.event.Order;

public class OrderCreatedPayload {

  @JsonProperty(value ="payload")
  private Order payload;

  private OrderCreatedPayload(Order payload) {
    this.payload = payload;

  }

  private OrderCreatedPayload(OrderCreatedPayloadBuilder builder) {
    this.payload = builder.payload;

  }

  public static class OrderCreatedPayloadBuilder {

    private Order payload;

    public OrderCreatedPayload.OrderCreatedPayloadBuilder payload(Order payload) {
      this.payload = payload;
      return this;
    }

    public OrderCreatedPayload build() {
      OrderCreatedPayload orderCreatedPayload = new OrderCreatedPayload(this);
      return orderCreatedPayload;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public Order getPayload() {
    return payload;
  }
  public void setPayload(Order payload) {
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
    OrderCreatedPayload orderCreatedPayload = (OrderCreatedPayload) o;
    return Objects.equals(this.payload, orderCreatedPayload.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderCreatedPayload {\n");
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
