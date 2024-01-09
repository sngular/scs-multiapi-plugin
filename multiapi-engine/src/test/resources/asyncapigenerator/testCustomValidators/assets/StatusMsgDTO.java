package com.sngular.scsplugin.customvalidator.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = StatusMsgDTO.StatusMsgDTOBuilder.class)
public class StatusMsgDTO {

  @JsonProperty(value ="clientId")
  private Integer clientId;
  @JsonProperty(value ="status")
  private Status status;
  public enum Status {
    OK("OK"),
    KO("KO");

    private String value;

    Status(String value) {
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

  private StatusMsgDTO(StatusMsgDTOBuilder builder) {
    this.clientId = builder.clientId;
    this.status = builder.status;

  }

  public static StatusMsgDTO.StatusMsgDTOBuilder builder() {
    return new StatusMsgDTO.StatusMsgDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class StatusMsgDTOBuilder {

    private Integer clientId;

    private Status status;

    public StatusMsgDTO.StatusMsgDTOBuilder clientId(Integer clientId) {
      this.clientId = clientId;
      return this;
    }

    public StatusMsgDTO.StatusMsgDTOBuilder status(Status status) {
      this.status = status;
      return this;
    }

    public StatusMsgDTO build() {
      StatusMsgDTO statusMsgDTO = new StatusMsgDTO(this);
      return statusMsgDTO;
    }
  }

  /**
  * Get clientId
  * @return clientId
  */
  @Schema(name = "clientId", required = false)
  public Integer getClientId() {
    return clientId;
  }
  public void setClientId(Integer clientId) {
    this.clientId = clientId;
  }

  /**
  * Get status
  * @return status
  */
  @Schema(name = "status", required = false)
  public Status getStatus() {
    return status;
  }
  public void setStatus(Status status) {
    this.status = status;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    StatusMsgDTO statusMsgDTO = (StatusMsgDTO) o;
    return Objects.equals(this.clientId, statusMsgDTO.clientId) && Objects.equals(this.status, statusMsgDTO.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientId, status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("StatusMsgDTO{");
    sb.append(" clientId:").append(clientId).append(",");
    sb.append(" status:").append(status);
    sb.append("}");
    return sb.toString();
  }


}
