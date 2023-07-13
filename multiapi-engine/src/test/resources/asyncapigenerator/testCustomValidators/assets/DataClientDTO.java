package com.sngular.scsplugin.customvalidator.model.event.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = DataClientDTO.DataClientDTOBuilder.class)
public class DataClientDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.customvalidator.model.event.schemas.DataDTO payload;

  private DataClientDTO(com.sngular.scsplugin.customvalidator.model.event.schemas.DataDTO payload) {
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

    private com.sngular.scsplugin.customvalidator.model.event.schemas.DataDTO payload;

    public DataClientDTO.DataClientDTOBuilder payload(com.sngular.scsplugin.customvalidator.model.event.schemas.DataDTO payload) {
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
  public com.sngular.scsplugin.customvalidator.model.event.schemas.DataDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.customvalidator.model.event.schemas.DataDTO payload) {
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
    sb.append("DataClientDTO{");
    sb.append(" payload:").append(payload).append(",");
    sb.append("}");
    return sb.toString();
  }




}
