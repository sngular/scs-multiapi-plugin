package net.coru.scsplugin.business_model.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.business_model.model.event.WaiterMapper;
import net.coru.scsplugin.business_model.model.event.OrderMapper;

public class CreateOrderPayloadMapper {

  @JsonProperty(value ="order")
  private OrderMapper order;
  @JsonProperty(value ="waiter")
  private WaiterMapper waiter;

  private CreateOrderPayloadMapper(OrderMapper order, WaiterMapper waiter) {
    this.order = order;
    this.waiter = waiter;

  }

  private CreateOrderPayloadMapper(CreateOrderPayloadMapperBuilder builder) {
    this.order = builder.order;
    this.waiter = builder.waiter;

  }

  public static CreateOrderPayloadMapper.CreateOrderPayloadMapperBuilder builder() {
    return new CreateOrderPayloadMapper.CreateOrderPayloadMapperBuilder();
  }

  public static class CreateOrderPayloadMapperBuilder {

    private OrderMapper order;
    private WaiterMapper waiter;

    public CreateOrderPayloadMapper.CreateOrderPayloadMapperBuilder order(OrderMapper order) {
      this.order = order;
      return this;
    }

    public CreateOrderPayloadMapper.CreateOrderPayloadMapperBuilder waiter(WaiterMapper waiter) {
      this.waiter = waiter;
      return this;
    }

    public CreateOrderPayloadMapper build() {
      CreateOrderPayloadMapper createOrderPayloadMapper = new CreateOrderPayloadMapper(this);
      return createOrderPayloadMapper;
    }
  }

  /**
  * Get order
  * @return order
  */
  @Schema(name = "order", required = false)
  public OrderMapper getOrder() {
    return order;
  }
  public void setOrder(OrderMapper order) {
    this.order = order;
  }

  /**
  * Get waiter
  * @return waiter
  */
  @Schema(name = "waiter", required = false)
  public WaiterMapper getWaiter() {
    return waiter;
  }
  public void setWaiter(WaiterMapper waiter) {
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
    CreateOrderPayloadMapper createOrderPayloadMapper = (CreateOrderPayloadMapper) o;
    return Objects.equals(this.order, createOrderPayloadMapper.order) && Objects.equals(this.waiter, createOrderPayloadMapper.waiter);
  }

  @Override
  public int hashCode() {
    return Objects.hash(order, waiter);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CreateOrderPayloadMapper {\n");
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
