package com.sngular.scsplugin.issuegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

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

  private StatusMsgDTO(Integer clientId, Status status) {
    this.clientId = clientId;
    this.status = status;

  }

  private StatusMsgDTO(StatusMsgDTOBuilder builder) {
    this.clientId = builder.clientId;
    this.status = builder.status;

  }

  public static StatusMsgDTO.StatusMsgDTOBuilder builder() {
    return new StatusMsgDTO.StatusMsgDTOBuilder();
  }

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
    sb.append("class StatusMsgDTO {\n");
    sb.append(" clientId: ").append(toIndentedString(clientId)).append("\n");
    sb.append(" status: ").append(toIndentedString(status)).append("\n");
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
