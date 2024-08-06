package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder.class)
public class CustomerEventPayloadDTO {

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
  @JsonProperty(value ="customerId")
  private String customerId;
  @JsonProperty(value ="customer")
  private CustomerDTO customer;

  private CustomerEventPayloadDTO(CustomerEventPayloadDTOBuilder builder) {
    this.eventType = builder.eventType;
    this.customerId = builder.customerId;
    this.customer = builder.customer;

  }

  public static CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder builder() {
    return new CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerEventPayloadDTOBuilder {

    private EventType eventType;
    private String customerId;
    private CustomerDTO customer;
    public CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder eventType(EventType eventType) {
      this.eventType = eventType;
      return this;
    }

    public CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder customerId(String customerId) {
      this.customerId = customerId;
      return this;
    }
    public CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder customer(CustomerDTO customer) {
      this.customer = customer;
      return this;
    }

    public CustomerEventPayloadDTO build() {
      CustomerEventPayloadDTO customerEventPayloadDTO = new CustomerEventPayloadDTO(this);
      return customerEventPayloadDTO;
    }
  }

  @Schema(name = "eventType", required = false)
  public EventType getEventType() {
    return eventType;
  }
  public void setEventType(EventType eventType) {
    this.eventType = eventType;
  }

  @Schema(name = "customerId", required = false)
  public String getCustomerId() {
    return customerId;
  }
  public void setCustomerId(String customerId) {
    this.customerId = customerId;
  }

  @Schema(name = "customer", required = false)
  public CustomerDTO getCustomer() {
    return customer;
  }
  public void setCustomer(CustomerDTO customer) {
    this.customer = customer;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CustomerEventPayloadDTO customerEventPayloadDTO = (CustomerEventPayloadDTO) o;
    return Objects.equals(this.eventType, customerEventPayloadDTO.eventType) && Objects.equals(this.customerId, customerEventPayloadDTO.customerId) && Objects.equals(this.customer, customerEventPayloadDTO.customer);
  }

  @Override
  public int hashCode() {
    return Objects.hash(eventType, customerId, customer);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerEventPayloadDTO{");
    sb.append(" eventType:").append(eventType).append(",");
    sb.append(" customerId:").append(customerId).append(",");
    sb.append(" customer:").append(customer);
    sb.append("}");
    return sb.toString();
  }


}
