package com.sngular.scsplugin.customvalidator.model.event.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = StatusDTO.StatusDTOBuilder.class)
public class StatusDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.customvalidator.model.event.schemas.StatusMsgDTO payload;

  private StatusDTO(com.sngular.scsplugin.customvalidator.model.event.schemas.StatusMsgDTO payload) {
    this.payload = payload;

  }

  private StatusDTO(StatusDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static StatusDTO.StatusDTOBuilder builder() {
    return new StatusDTO.StatusDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class StatusDTOBuilder {

    private com.sngular.scsplugin.customvalidator.model.event.schemas.StatusMsgDTO payload;

    public StatusDTO.StatusDTOBuilder payload(com.sngular.scsplugin.customvalidator.model.event.schemas.StatusMsgDTO payload) {
      this.payload = payload;
      return this;
    }

    public StatusDTO build() {
      StatusDTO statusDTO = new StatusDTO(this);
      return statusDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public com.sngular.scsplugin.customvalidator.model.event.schemas.StatusMsgDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.customvalidator.model.event.schemas.StatusMsgDTO payload) {
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
    StatusDTO statusDTO = (StatusDTO) o;
    return Objects.equals(this.payload, statusDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("StatusDTO{");
    sb.append(" payload:").append(payload);
    sb.append("}");
    return sb.toString();
  }




}
