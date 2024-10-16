package com.sngular.scsplugin.issuegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = StatusMsgDTO.StatusMsgDTOBuilder.class)
public class StatusMsgDTO {

  @JsonProperty(value ="status")
  private Status status;
  public enum Status {
    KO("KO"),
    OK("OK");

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
  @JsonProperty(value ="clientId")
  private Integer clientId;

  private StatusMsgDTO(StatusMsgDTOBuilder builder) {
    this.status = builder.status;
    this.clientId = builder.clientId;

  }

  public static StatusMsgDTO.StatusMsgDTOBuilder builder() {
    return new StatusMsgDTO.StatusMsgDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class StatusMsgDTOBuilder {

    private Status status;
    private Integer clientId;

    public StatusMsgDTO.StatusMsgDTOBuilder status(Status status) {
      this.status = status;
      return this;
    }

    public StatusMsgDTO.StatusMsgDTOBuilder clientId(Integer clientId) {
      this.clientId = clientId;
      return this;
    }

    public StatusMsgDTO build() {
      StatusMsgDTO statusMsgDTO = new StatusMsgDTO(this);
      return statusMsgDTO;
    }
  }

  @Schema(name = "status", required = false)
  public Status getStatus() {
    return status;
  }
  public void setStatus(Status status) {
    this.status = status;
  }

  @Schema(name = "clientId", required = false)
  public Integer getClientId() {
    return clientId;
  }
  public void setClientId(Integer clientId) {
    this.clientId = clientId;
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
    return Objects.equals(this.status, statusMsgDTO.status) && Objects.equals(this.clientId, statusMsgDTO.clientId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(status, clientId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("StatusMsgDTO{");
    sb.append(" status:").append(status).append(",");
    sb.append(" clientId:").append(clientId);
    sb.append("}");
    return sb.toString();
  }


}
