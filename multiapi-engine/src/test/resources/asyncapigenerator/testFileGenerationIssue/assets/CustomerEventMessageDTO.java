package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.filegenerationissue.model.event.CustomerEventPayloadDTO;

@JsonDeserialize(builder = CustomerEventMessageDTO.CustomerEventMessageDTOBuilder.class)
public class CustomerEventMessageDTO {

  @JsonProperty(value ="payload")
  private CustomerEventPayloadDTO payload;

  private CustomerEventMessageDTO(CustomerEventPayloadDTO payload) {
    this.payload = payload;

  }

  private CustomerEventMessageDTO(CustomerEventMessageDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static CustomerEventMessageDTO.CustomerEventMessageDTOBuilder builder() {
    return new CustomerEventMessageDTO.CustomerEventMessageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerEventMessageDTOBuilder {

    private CustomerEventPayloadDTO payload;

    public CustomerEventMessageDTO.CustomerEventMessageDTOBuilder payload(CustomerEventPayloadDTO payload) {
      this.payload = payload;
      return this;
    }

    public CustomerEventMessageDTO build() {
      CustomerEventMessageDTO customerEventMessageDTO = new CustomerEventMessageDTO(this);
      return customerEventMessageDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public CustomerEventPayloadDTO getPayload() {
    return payload;
  }
  public void setPayload(CustomerEventPayloadDTO payload) {
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
    CustomerEventMessageDTO customerEventMessageDTO = (CustomerEventMessageDTO) o;
    return Objects.equals(this.payload, customerEventMessageDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CustomerEventMessageDTO {\n");
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
