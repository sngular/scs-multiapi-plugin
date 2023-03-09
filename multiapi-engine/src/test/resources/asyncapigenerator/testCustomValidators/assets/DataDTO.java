package com.sngular.scsplugin.customvalidator.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.customvalidator.model.event.Integer;
import java.util.List;
import java.util.ArrayList;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Size;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Max;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Min;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.MaxItems;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.MinItems;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Pattern;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.MultipleOf;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.NotNull;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.UniqueItems;

public class DataDTO {

  @JsonProperty(value ="clientId")
  @Min(minimum = 10)
  @Max(maximum = 200, exclusive = true)
  @MultipleOf(multiple = "10.55")
  @NotNull
  private Integer clientId;
  @JsonProperty(value ="clientName")
  @Size(min =50, max =200)
  @Pattern(regex = "^[a-zA-Z0-9_.-]*$")
  @NotNull
  private String clientName;
  @JsonProperty(value ="flightNumber")
  private String flightNumber;
  @JsonProperty(value ="test")
  @MaxItems(maximum = 10)
  @MinItems(minimum = 5)
  @UniqueItems
  @NotNull
  private List<integer> test = new ArrayList<integer>();

  private DataDTO(Integer clientId, String clientName, String flightNumber, List<integer> test) {
    this.clientId = clientId;
    this.clientName = clientName;
    this.flightNumber = flightNumber;
    this.test = test;

  }

  private DataDTO(DataDTOBuilder builder) {
    this.clientId = builder.clientId;
    this.clientName = builder.clientName;
    this.flightNumber = builder.flightNumber;
    this.test = builder.test;

  }

  public static DataDTO.DataDTOBuilder builder() {
    return new DataDTO.DataDTOBuilder();
  }

  public static class DataDTOBuilder {

    private Integer clientId;
    private String clientName;
    private String flightNumber;
    private List<integer> test = new ArrayList<integer>();

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
    public DataDTO.DataDTOBuilder test(List<integer> test) {
      if (!test.isEmpty()) {
        this.test.addAll(test);
      }
      return this;
    }

    public DataDTO.DataDTOBuilder tes(integer tes) {
      if (tes != null) {
        this.test.add(tes);
      }
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

  /**
  * Get test
  * @return test
  */
  @Schema(name = "test", required = false)
  public List<integer> getTest() {
    return test;
  }
  public void setTest(List<integer> test) {
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
    return Objects.equals(this.clientId, dataDTO.clientId) && Objects.equals(this.clientName, dataDTO.clientName) && Objects.equals(this.flightNumber, dataDTO.flightNumber) && Objects.equals(this.test, dataDTO.test);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientId, clientName, flightNumber, test);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DataDTO {\n");
    sb.append(" clientId: ").append(toIndentedString(clientId)).append("\n");
    sb.append(" clientName: ").append(toIndentedString(clientName)).append("\n");
    sb.append(" flightNumber: ").append(toIndentedString(flightNumber)).append("\n");
    sb.append(" test: ").append(toIndentedString(test)).append("\n");
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
