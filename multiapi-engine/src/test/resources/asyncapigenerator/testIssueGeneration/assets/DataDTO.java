package com.sngular.scsplugin.issuegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = DataDTO.DataDTOBuilder.class)
public class DataDTO {

  @JsonProperty(value ="clientId")
  private Integer clientId;
  @JsonProperty(value ="clientName")
  private String clientName;
  @JsonProperty(value ="flightNumber")
  private String flightNumber;

  private DataDTO(DataDTOBuilder builder) {
    this.clientId = builder.clientId;
    this.clientName = builder.clientName;
    this.flightNumber = builder.flightNumber;

  }

  public static DataDTO.DataDTOBuilder builder() {
    return new DataDTO.DataDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DataDTOBuilder {

    private Integer clientId;
    private String clientName;
    private String flightNumber;

    public DataDTO.DataDTOBuilder clientId(Integer clientId) {
      this.clientId = clientId;
      return this;
    }

    public DataDTO.DataDTOBuilder clientName(String clientName) {
      this.clientName = clientName;
      return this;
    }

    public DataDTO.DataDTOBuilder flightNumber(String flightNumber) {
      this.flightNumber = flightNumber;
      return this;
    }

    public DataDTO build() {
      DataDTO dataDTO = new DataDTO(this);
      return dataDTO;
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
  * Get clientName
  * @return clientName
  */
  @Schema(name = "clientName", required = false)
  public String getClientName() {
    return clientName;
  }
  public void setClientName(String clientName) {
    this.clientName = clientName;
  }

  /**
  * Get flightNumber
  * @return flightNumber
  */
  @Schema(name = "flightNumber", required = false)
  public String getFlightNumber() {
    return flightNumber;
  }
  public void setFlightNumber(String flightNumber) {
    this.flightNumber = flightNumber;
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
    return Objects.equals(this.clientId, dataDTO.clientId) && Objects.equals(this.clientName, dataDTO.clientName) && Objects.equals(this.flightNumber, dataDTO.flightNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientId, clientName, flightNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("DataDTO{");
    sb.append(" clientId:").append(clientId).append(",");
    sb.append(" clientName:").append(clientName).append(",");
    sb.append(" flightNumber:").append(flightNumber);
    sb.append("}");
    return sb.toString();
  }


}
