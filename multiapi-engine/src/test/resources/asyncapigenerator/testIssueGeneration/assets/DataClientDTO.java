package com.sngular.scsplugin.issuegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.issuegeneration.model.event.DataDTO;

@JsonDeserialize(builder = DataClientDTO.DataClientDTOBuilder.class)
public class DataClientDTO {

  @JsonProperty(value ="payload")
  private DataDTO payload;

  private DataClientDTO(DataDTO payload) {
    this.payload = payload;

  }

  private DataClientDTO(DataClientDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static DataClientDTO.DataClientDTOBuilder builder() {
    return new DataClientDTO.DataClientDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DataClientDTOBuilder {

    private DataDTO payload;

    public DataClientDTO.DataClientDTOBuilder payload(DataDTO payload) {
      this.payload = payload;
      return this;
    }

    public DataClientDTO build() {
      DataClientDTO dataClientDTO = new DataClientDTO(this);
      return dataClientDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public DataDTO getPayload() {
    return payload;
  }
  public void setPayload(DataDTO payload) {
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
    DataClientDTO dataClientDTO = (DataClientDTO) o;
    return Objects.equals(this.payload, dataClientDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DataClientDTO {\n");
    sb.append(" payload: ").append(toIndentedString(payload)).append("\n");
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
