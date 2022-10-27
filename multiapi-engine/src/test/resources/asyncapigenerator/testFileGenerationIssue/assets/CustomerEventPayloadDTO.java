package net.coru.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

public class CustomerEventPayloadDTO {

  @JsonProperty(value ="customerId")
  private String customerId;
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
  @JsonProperty(value ="customerDTO")
  private CustomerDTO customerDTO;

  private CustomerEventPayloadDTO(String customerId, EventType eventType, CustomerDTO customerDTO) {
    this.customerId = customerId;
    this.eventType = eventType;
    this.customerDTO = customerDTO;

  }

  private CustomerEventPayloadDTO(CustomerEventPayloadDTOBuilder builder) {
    this.customerId = builder.customerId;
    this.eventType = builder.eventType;
    this.customerDTO = builder.customerDTO;

  }

  public static CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder builder() {
    return new CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder();
  }

  public static class CustomerEventPayloadDTOBuilder {

    private String customerId;
    private EventType eventType;
    private CustomerDTO customerDTO;

    public CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder customerId(String customerId) {
      this.customerId = customerId;
      return this;
    }
    public CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder eventType(EventType eventType) {
      this.eventType = eventType;
      return this;
    }

    public CustomerEventPayloadDTO.CustomerEventPayloadDTOBuilder customerDTO(CustomerDTO customerDTO) {
      this.customerDTO = customerDTO;
      return this;
    }

    public CustomerEventPayloadDTO build() {
      CustomerEventPayloadDTO customerEventPayloadDTO = new CustomerEventPayloadDTO(this);
      return customerEventPayloadDTO;
    }
  }

  /**
  * Get customerId
  * @return customerId
  */
  @Schema(name = "customerId", required = false)
  public String getCustomerId() {
    return customerId;
  }
  public void setCustomerId(String customerId) {
    this.customerId = customerId;
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
  * Get customerDTO
  * @return customerDTO
  */
  @Schema(name = "customerDTO", required = false)
  public CustomerDTO getCustomerDTO() {
    return customerDTO;
  }
  public void setCustomerDTO(CustomerDTO customerDTO) {
    this.customerDTO = customerDTO;
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
    return Objects.equals(this.customerId, customerEventPayloadDTO.customerId) && Objects.equals(this.eventType, customerEventPayloadDTO.eventType) && Objects.equals(this.customerDTO, customerEventPayloadDTO.customerDTO);
  }

  @Override
  public int hashCode() {
    return Objects.hash(customerId, eventType, customerDTO);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CustomerEventPayloadDTO {\n");
    sb.append(" customerId: ").append(toIndentedString(customerId)).append("\n");
    sb.append(" eventType: ").append(toIndentedString(eventType)).append("\n");
    sb.append(" customerDTO: ").append(toIndentedString(customerDTO)).append("\n");
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
