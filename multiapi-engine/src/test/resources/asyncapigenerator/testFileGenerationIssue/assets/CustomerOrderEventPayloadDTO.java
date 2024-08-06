package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder.class)
public class CustomerOrderEventPayloadDTO {

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
  @JsonProperty(value ="customerOrderId")
  private String customerOrderId;

  private CustomerOrderEventPayloadDTO(CustomerOrderEventPayloadDTOBuilder builder) {
    this.eventType = builder.eventType;
    this.customerOrder = builder.customerOrder;
    this.customerOrderId = builder.customerOrderId;

  }

  public static CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder builder() {
    return new CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerOrderEventPayloadDTOBuilder {

    private EventType eventType;
    private CustomerOrderDTO customerOrder;
    private String customerOrderId;
    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder eventType(EventType eventType) {
      this.eventType = eventType;
      return this;
    }
    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder customerOrder(CustomerOrderDTO customerOrder) {
      this.customerOrder = customerOrder;
      return this;
    }

    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder customerOrderId(String customerOrderId) {
      this.customerOrderId = customerOrderId;
      return this;
    }

    public CustomerOrderEventPayloadDTO build() {
      CustomerOrderEventPayloadDTO customerOrderEventPayloadDTO = new CustomerOrderEventPayloadDTO(this);
      return customerOrderEventPayloadDTO;
    }
  }

  @Schema(name = "eventType", required = false)
  public EventType getEventType() {
    return eventType;
  }
  public void setEventType(EventType eventType) {
    this.eventType = eventType;
  }

  @Schema(name = "customerOrder", required = false)
  public CustomerOrderDTO getCustomerOrder() {
    return customerOrder;
  }
  public void setCustomerOrder(CustomerOrderDTO customerOrder) {
    this.customerOrder = customerOrder;
  }

  @Schema(name = "customerOrderId", required = false)
  public String getCustomerOrderId() {
    return customerOrderId;
  }
  public void setCustomerOrderId(String customerOrderId) {
    this.customerOrderId = customerOrderId;
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
    return Objects.equals(this.eventType, customerOrderEventPayloadDTO.eventType) && Objects.equals(this.customerOrder, customerOrderEventPayloadDTO.customerOrder) && Objects.equals(this.customerOrderId, customerOrderEventPayloadDTO.customerOrderId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(eventType, customerOrder, customerOrderId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerOrderEventPayloadDTO{");
    sb.append(" eventType:").append(eventType).append(",");
    sb.append(" customerOrder:").append(customerOrder).append(",");
    sb.append(" customerOrderId:").append(customerOrderId);
    sb.append("}");
    return sb.toString();
  }


}
