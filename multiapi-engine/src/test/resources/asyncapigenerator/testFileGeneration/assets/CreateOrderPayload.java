package net.coru.scsplugin.business_model.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.business_model.model.event.Order;
import net.coru.scsplugin.business_model.model.event.Waiter;

public class CreateOrderPayload {

  @JsonProperty(value ="order")
  private Order order;
  @JsonProperty(value ="waiter")
  private Waiter waiter;

  private CreateOrderPayload(Order order, Waiter waiter) {
    this.order = order;
    this.waiter = waiter;

  }

  private CreateOrderPayload(CreateOrderPayloadBuilder builder) {
    this.order = builder.order;
    this.waiter = builder.waiter;

  }

  public static class CreateOrderPayloadBuilder {

    private Order order;
    private Waiter waiter;

    public CreateOrderPayload.CreateOrderPayloadBuilder order(Order order) {
      this.order = order;
      return this;
    }

    public CreateOrderPayload.CreateOrderPayloadBuilder waiter(Waiter waiter) {
      this.waiter = waiter;
      return this;
    }

    public CreateOrderPayload build() {
      CreateOrderPayload createOrderPayload = new CreateOrderPayload(this);
      return createOrderPayload;
    }
  }

  /**
  * Get order
  * @return order
  */
  @Schema(name = "order", required = false)
  public Order getOrder() {
    return order;
  }
  public void setOrder(Order order) {
    this.order = order;
  }

  /**
  * Get waiter
  * @return waiter
  */
  @Schema(name = "waiter", required = false)
  public Waiter getWaiter() {
    return waiter;
  }
  public void setWaiter(Waiter waiter) {
    this.waiter = waiter;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CreateOrderPayload createOrderPayload = (CreateOrderPayload) o;
    return Objects.equals(this.order, createOrderPayload.order) && Objects.equals(this.waiter, createOrderPayload.waiter);
  }

  @Override
  public int hashCode() {
    return Objects.hash(order, waiter);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CreateOrderPayload {\n");
    sb.append(" order: ").append(toIndentedString(order)).append("\n");
    sb.append(" waiter: ").append(toIndentedString(waiter)).append("\n");
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
