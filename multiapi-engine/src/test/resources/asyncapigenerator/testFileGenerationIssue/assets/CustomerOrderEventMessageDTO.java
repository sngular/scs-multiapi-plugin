package com.sngular.scsplugin.filegenerationissue.model.event.message;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.filegenerationissue.model.event.schema.CustomerOrderEventPayloadDTO;

@JsonDeserialize(builder = CustomerOrderEventMessageDTO.CustomerOrderEventMessageDTOBuilder.class)
public class CustomerOrderEventMessageDTO {

  @JsonProperty(value ="payload")
  private CustomerOrderEventPayloadDTO payload;

  private CustomerOrderEventMessageDTO(CustomerOrderEventPayloadDTO payload) {
    this.payload = payload;

  }

  private CustomerOrderEventMessageDTO(CustomerOrderEventMessageDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static CustomerOrderEventMessageDTO.CustomerOrderEventMessageDTOBuilder builder() {
    return new CustomerOrderEventMessageDTO.CustomerOrderEventMessageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerOrderEventMessageDTOBuilder {

    private CustomerOrderEventPayloadDTO payload;

    public CustomerOrderEventMessageDTO.CustomerOrderEventMessageDTOBuilder payload(CustomerOrderEventPayloadDTO payload) {
      this.payload = payload;
      return this;
    }

    public CustomerOrderEventMessageDTO build() {
      CustomerOrderEventMessageDTO customerOrderEventMessageDTO = new CustomerOrderEventMessageDTO(this);
      return customerOrderEventMessageDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public CustomerOrderEventPayloadDTO getPayload() {
    return payload;
  }
  public void setPayload(CustomerOrderEventPayloadDTO payload) {
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
    CustomerOrderEventMessageDTO customerOrderEventMessageDTO = (CustomerOrderEventMessageDTO) o;
    return Objects.equals(this.payload, customerOrderEventMessageDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CustomerOrderEventMessageDTO {\n");
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
