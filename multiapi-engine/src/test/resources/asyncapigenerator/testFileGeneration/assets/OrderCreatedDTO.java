package net.coru.scsplugin.business_model.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.business_model.model.event.OrderCreatedPayloadDTO;
import net.coru.scsplugin.business_model.model.event.exception.ModelClassException;

public class OrderCreatedDTO {

  @JsonProperty(value ="payload")
  private final OrderCreatedPayloadDTO payload;

  private OrderCreatedDTO(OrderCreatedPayloadDTO payload) {
    this.payload = payload;

    validateRequiredAttributes();
  }

  private OrderCreatedDTO(OrderCreatedDTOBuilder builder) {
    this.payload = builder.payload;

    validateRequiredAttributes();
  }

  public static OrderCreatedDTO.OrderCreatedDTOBuilder builder() {
    return new OrderCreatedDTO.OrderCreatedDTOBuilder();
  }

  public static class OrderCreatedDTOBuilder {

    private OrderCreatedPayloadDTO payload;

    public OrderCreatedDTO.OrderCreatedDTOBuilder payload(OrderCreatedPayloadDTO payload) {
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
  @Schema(name = "payload", required = true)
  public OrderCreatedPayloadDTO getPayload() {
    return payload;
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
    sb.append("class OrderCreatedDTO {\n");
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


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.payload)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderCreatedDTO");
    }
  }

}
