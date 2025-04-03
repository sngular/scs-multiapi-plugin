package com.sngular.scsplugin.issuesimpletypegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = DataDTO.DataDTOBuilder.class)
public class DataDTO {

  @JsonProperty(value ="clientName")
  private String clientName;
  @JsonProperty(value ="flightNumber")
  private String flightNumber;
  @JsonProperty(value ="clientId")
  private Object clientId;

  private DataDTO(DataDTOBuilder builder) {
    this.clientName = builder.clientName;
    this.flightNumber = builder.flightNumber;
    this.clientId = builder.clientId;

  }

  public static DataDTO.DataDTOBuilder builder() {
    return new DataDTO.DataDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DataDTOBuilder {

    private String clientName;
    private String flightNumber;
    private Object clientId;

    public DataDTO.DataDTOBuilder clientName(String clientName) {
      this.clientName = clientName;
      return this;
    }

    public DataDTO.DataDTOBuilder flightNumber(String flightNumber) {
      this.flightNumber = flightNumber;
      return this;
    }

    public DataDTO.DataDTOBuilder clientId(Object clientId) {
      this.clientId = clientId;
      return this;
    }

    public DataDTO build() {
      DataDTO dataDTO = new DataDTO(this);
      return dataDTO;
    }
  }

  @Schema(name = "clientName", required = false)
  public String getClientName() {
    return clientName;
  }
  public void setClientName(String clientName) {
    this.clientName = clientName;
  }

  @Schema(name = "flightNumber", required = false)
  public String getFlightNumber() {
    return flightNumber;
  }
  public void setFlightNumber(String flightNumber) {
    this.flightNumber = flightNumber;
  }

  @Schema(name = "clientId", required = false)
  public Object getClientId() {
    return clientId;
  }
  public void setClientId(Object clientId) {
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
    DataDTO dataDTO = (DataDTO) o;
    return Objects.equals(this.clientName, dataDTO.clientName) && Objects.equals(this.flightNumber, dataDTO.flightNumber) && Objects.equals(this.clientId, dataDTO.clientId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientName, flightNumber, clientId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("DataDTO{");
    sb.append(" clientName:").append(clientName).append(",");
    sb.append(" flightNumber:").append(flightNumber).append(",");
    sb.append(" clientId:").append(clientId);
    sb.append("}");
    return sb.toString();
  }


}
