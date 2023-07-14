package com.sngular.scsplugin.filegenerationissue.model.event.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CustomerOrderEventMessageDTO.CustomerOrderEventMessageDTOBuilder.class)
public class CustomerOrderEventMessageDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderEventPayloadDTO payload;

  private CustomerOrderEventMessageDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderEventPayloadDTO payload) {
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

    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderEventPayloadDTO payload;

    public CustomerOrderEventMessageDTO.CustomerOrderEventMessageDTOBuilder payload(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderEventPayloadDTO payload) {
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
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderEventPayloadDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderEventPayloadDTO payload) {
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
    sb.append("CustomerOrderEventMessageDTO{");
    sb.append(" payload:").append(payload).append(",");
    sb.append("}");
    return sb.toString();
  }




}
