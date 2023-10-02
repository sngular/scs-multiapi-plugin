package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder.class)
public class CustomerOrderEventPayloadDTO {

  @JsonProperty(value ="customerOrderId")
  private String customerOrderId;
  @JsonProperty(value ="eventType")
  private EventType eventType;
  public enum EventType {
    CREATED("created"),
    UPDATED("updated"),
    DELETED("deleted");

    private String value;

    EventType(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="customerOrder")
  private CustomerOrderDTO customerOrder;

  private CustomerOrderEventPayloadDTO(String customerOrderId, EventType eventType, CustomerOrderDTO customerOrder) {
    this.customerOrderId = customerOrderId;
    this.eventType = eventType;
    this.customerOrder = customerOrder;

  }

  private CustomerOrderEventPayloadDTO(CustomerOrderEventPayloadDTOBuilder builder) {
    this.customerOrderId = builder.customerOrderId;
    this.eventType = builder.eventType;
    this.customerOrder = builder.customerOrder;

  }

  public static CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder builder() {
    return new CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerOrderEventPayloadDTOBuilder {

    private String customerOrderId;
    private EventType eventType;
    private CustomerOrderDTO customerOrder;

    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder customerOrderId(String customerOrderId) {
      this.customerOrderId = customerOrderId;
      return this;
    }

    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder eventType(EventType eventType) {
      this.eventType = eventType;
      return this;
    }

    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder customerOrder(CustomerOrderDTO customerOrder) {
      this.customerOrder = customerOrder;
      return this;
    }

    public CustomerOrderEventPayloadDTO build() {
      CustomerOrderEventPayloadDTO customerOrderEventPayloadDTO = new CustomerOrderEventPayloadDTO(this);
      return customerOrderEventPayloadDTO;
    }
  }

  /**
  * Get customerOrderId
  * @return customerOrderId
  */
  @Schema(name = "customerOrderId", required = false)
  public String getCustomerOrderId() {
    return customerOrderId;
  }
  public void setCustomerOrderId(String customerOrderId) {
    this.customerOrderId = customerOrderId;
  }

  /**
  * Get eventType
  * @return eventType
  */
  @Schema(name = "eventType", required = false)
  public EventType getEventType() {
    return eventType;
  }
  public void setEventType(EventType eventType) {
    this.eventType = eventType;
  }

  /**
  * Get customerOrder
  * @return customerOrder
  */
  @Schema(name = "customerOrder", required = false)
  public CustomerOrderDTO getCustomerOrder() {
    return customerOrder;
  }
  public void setCustomerOrder(CustomerOrderDTO customerOrder) {
    this.customerOrder = customerOrder;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CustomerOrderEventPayloadDTO customerOrderEventPayloadDTO = (CustomerOrderEventPayloadDTO) o;
    return Objects.equals(this.customerOrderId, customerOrderEventPayloadDTO.customerOrderId) && Objects.equals(this.eventType, customerOrderEventPayloadDTO.eventType) && Objects.equals(this.customerOrder, customerOrderEventPayloadDTO.customerOrder);
  }

  @Override
  public int hashCode() {
    return Objects.hash(customerOrderId, eventType, customerOrder);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerOrderEventPayloadDTO{");
    sb.append(" customerOrderId:").append(customerOrderId).append(",");
    sb.append(" eventType:").append(eventType).append(",");
    sb.append(" customerOrder:").append(customerOrder);
    sb.append("}");
    return sb.toString();
  }


}
