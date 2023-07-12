package com.sngular.scsplugin.filegenerationissue.model.event.schemas;

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
  @JsonProperty(value ="customerOrderDTO")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderDTO customerOrderDTO;

  private CustomerOrderEventPayloadDTO(String customerOrderId, EventType eventType, com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderDTO customerOrderDTO) {
    this.customerOrderId = customerOrderId;
    this.eventType = eventType;
    this.customerOrderDTO = customerOrderDTO;

  }

  private CustomerOrderEventPayloadDTO(CustomerOrderEventPayloadDTOBuilder builder) {
    this.customerOrderId = builder.customerOrderId;
    this.eventType = builder.eventType;
    this.customerOrderDTO = builder.customerOrderDTO;

  }

  public static CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder builder() {
    return new CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerOrderEventPayloadDTOBuilder {

    private String customerOrderId;
    private EventType eventType;
    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderDTO customerOrderDTO;

    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder customerOrderId(String customerOrderId) {
      this.customerOrderId = customerOrderId;
      return this;
    }
    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder eventType(EventType eventType) {
      this.eventType = eventType;
      return this;
    }

    public CustomerOrderEventPayloadDTO.CustomerOrderEventPayloadDTOBuilder customerOrderDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderDTO customerOrderDTO) {
      this.customerOrderDTO = customerOrderDTO;
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
  * Get customerOrderDTO
  * @return customerOrderDTO
  */
  @Schema(name = "customerOrderDTO", required = false)
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderDTO getCustomerOrderDTO() {
    return customerOrderDTO;
  }
  public void setCustomerOrderDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderDTO customerOrderDTO) {
    this.customerOrderDTO = customerOrderDTO;
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
    return Objects.equals(this.customerOrderId, customerOrderEventPayloadDTO.customerOrderId) && Objects.equals(this.eventType, customerOrderEventPayloadDTO.eventType) && Objects.equals(this.customerOrderDTO, customerOrderEventPayloadDTO.customerOrderDTO);
  }

  @Override
  public int hashCode() {
    return Objects.hash(customerOrderId, eventType, customerOrderDTO);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerOrderEventPayloadDTO{");
    sb.append(" customerOrderId:").append(toIndentedString(customerOrderId)).append(",");
    sb.append(" eventType:").append(toIndentedString(eventType)).append(",");
    sb.append(" customerOrderDTO:").append(toIndentedString(customerOrderDTO)).append(",");
    sb.append("}");
    return sb.toString();
  }




}
