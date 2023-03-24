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
import com.sngular.scsplugin.customvalidator.model.event.exception.ModelClassException;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.Pattern;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.MultipleOf;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.NotNull;
import com.sngular.scsplugin.customvalidator.model.event.customvalidator.UniqueItems;

public class DataDTO {

  @JsonProperty(value ="clientId")
  @Min(minimum = "10", exclusive = false)
  @Max(maximum = "200", exclusive = true)
  @MultipleOf(multiple = "10")
  @NotNull
  private final Integer clientId;
  @JsonProperty(value ="clientName")
  @Size(min =50, max =200)
  @Pattern(regex = "^[a-zA-Z0-9_.-]*$")
  @NotNull
  private final String clientName;
  @JsonProperty(value ="flightNumber")
  @NotNull
  private final String flightNumber;
  @JsonProperty(value ="test")
  @MaxItems(maximum = 10)
  @MinItems(minimum = 5)
  @UniqueItems
  @NotNull
  private final List<integer> test;

  private DataDTO(Integer clientId, String clientName, String flightNumber, List<integer> test) {
    this.clientId = clientId;
    this.clientName = clientName;
    this.flightNumber = flightNumber;
    this.test = test;

    validateRequiredAttributes();
  }

  private DataDTO(DataDTOBuilder builder) {
    this.clientId = builder.clientId;
    this.clientName = builder.clientName;
    this.flightNumber = builder.flightNumber;
    this.test = builder.test;

    validateRequiredAttributes();
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
  @Schema(name = "clientId", required = true)
  public Integer getClientId() {
    return clientId;
  }

  /**
  * Get clientName
  * @return clientName
  */
  @Schema(name = "clientName", required = true)
  public String getClientName() {
    return clientName;
  }

  /**
  * Get flightNumber
  * @return flightNumber
  */
  @Schema(name = "flightNumber", required = true)
  public String getFlightNumber() {
    return flightNumber;
  }

  /**
  * Get test
  * @return test
  */
  @Schema(name = "test", required = true)
  public List<integer> getTest() {
    return test;
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


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.clientId)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.clientName)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.flightNumber)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.test)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("DataDTO");
    }
  }

}
