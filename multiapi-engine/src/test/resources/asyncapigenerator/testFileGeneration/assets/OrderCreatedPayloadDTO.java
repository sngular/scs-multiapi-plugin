package net.coru.scsplugin.business_model.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.business_model.model.event.OrderDTO;

public class OrderCreatedPayloadDTO {

  @JsonProperty(value ="payload")
  private OrderDTO payload;

  private OrderCreatedPayloadDTO(OrderDTO payload) {
    this.payload = payload;

  }

  private OrderCreatedPayloadDTO(OrderCreatedPayloadDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static OrderCreatedPayloadDTO.OrderCreatedPayloadDTOBuilder builder() {
    return new OrderCreatedPayloadDTO.OrderCreatedPayloadDTOBuilder();
  }

  public static class OrderCreatedPayloadDTOBuilder {

    private OrderDTO payload;

    public OrderCreatedPayloadDTO.OrderCreatedPayloadDTOBuilder payload(OrderDTO payload) {
      this.payload = payload;
      return this;
    }

    public OrderCreatedPayloadDTO build() {
      OrderCreatedPayloadDTO orderCreatedPayloadDTO = new OrderCreatedPayloadDTO(this);
      return orderCreatedPayloadDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public OrderDTO getPayload() {
    return payload;
  }
  public void setPayload(OrderDTO payload) {
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
    OrderCreatedPayloadDTO orderCreatedPayloadDTO = (OrderCreatedPayloadDTO) o;
    return Objects.equals(this.payload, orderCreatedPayloadDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderCreatedPayloadDTO {\n");
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
