package com.sngular.scsplugin.customvalidator.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Size;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Max;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Min;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.MaxItems;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.MinItems;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Pattern;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.MultipleOf;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.UniqueItems;

@JsonDeserialize(builder = DataDTO.DataDTOBuilder.class)
public class DataDTO {

  @JsonProperty(value ="clientName")
  @Size(min =50, max =200)
  @Pattern(regex = "^[a-zA-Z0-9_.-]*$")
  private String clientName;
  @JsonProperty(value ="flightNumber")
  private String flightNumber;
  @JsonProperty(value ="clientId")
  @Min(minimum = "10", exclusive = false)
  @Max(maximum = "200", exclusive = true)
  @MultipleOf(multiple = "10")
  private Integer clientId;
  @JsonProperty(value ="test")
  @MaxItems(maximum = 10)
  @MinItems(minimum = 5)
  @UniqueItems
  private List<Integer> test = new ArrayList<Integer>();

  private DataDTO(DataDTOBuilder builder) {
    this.clientName = builder.clientName;
    this.flightNumber = builder.flightNumber;
    this.clientId = builder.clientId;
    this.test = builder.test;

  }

  public static DataDTO.DataDTOBuilder builder() {
    return new DataDTO.DataDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DataDTOBuilder {

    private String clientName;
    private String flightNumber;
    private Integer clientId;
    private List<Integer> test = new ArrayList<Integer>();

    public DataDTO.DataDTOBuilder clientName(String clientName) {
      this.clientName = clientName;
      return this;
    }

    public DataDTO.DataDTOBuilder flightNumber(String flightNumber) {
      this.flightNumber = flightNumber;
      return this;
    }

    public DataDTO.DataDTOBuilder clientId(Integer clientId) {
      this.clientId = clientId;
      return this;
    }
    public DataDTO.DataDTOBuilder test(List<Integer> test) {
      if (!test.isEmpty()) {
        this.test.addAll(test);
      }
      return this;
    }

    public DataDTO.DataDTOBuilder test(Integer test) {
      if (test != null) {
        this.test.add(test);
      }
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
  public Integer getClientId() {
    return clientId;
  }
  public void setClientId(Integer clientId) {
    this.clientId = clientId;
  }

  @Schema(name = "test", required = false)
  public List<Integer> getTest() {
    return test;
  }
  public void setTest(List<Integer> test) {
    this.test = test;
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
    return Objects.equals(this.clientName, dataDTO.clientName) && Objects.equals(this.flightNumber, dataDTO.flightNumber) && Objects.equals(this.clientId, dataDTO.clientId) && Objects.equals(this.test, dataDTO.test);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientName, flightNumber, clientId, test);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("DataDTO{");
    sb.append(" clientName:").append(clientName).append(",");
    sb.append(" flightNumber:").append(flightNumber).append(",");
    sb.append(" clientId:").append(clientId).append(",");
    sb.append(" test:").append(test);
    sb.append("}");
    return sb.toString();
  }


}
