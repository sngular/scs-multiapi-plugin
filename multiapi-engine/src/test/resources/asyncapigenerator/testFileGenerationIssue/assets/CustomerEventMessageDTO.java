package com.sngular.scsplugin.filegenerationissue.model.event.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CustomerEventMessageDTO.CustomerEventMessageDTOBuilder.class)
public class CustomerEventMessageDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO payload;

  private CustomerEventMessageDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO payload) {
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

    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO payload;

    public CustomerEventMessageDTO.CustomerEventMessageDTOBuilder payload(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO payload) {
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
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO payload) {
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
    sb.append("CustomerEventMessageDTO{");
    sb.append(" payload:").append(toIndentedString(payload)).append(",");
    sb.append("}");
    return sb.toString();
  }




}
