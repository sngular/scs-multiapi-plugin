package com.sngular.multifileplugin.externalcomponentschemarefs.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = OpeningSlotDTO.OpeningSlotDTOBuilder.class)
public class OpeningSlotDTO {

  @JsonProperty(value ="day")
  private String day;

  private OpeningSlotDTO(OpeningSlotDTOBuilder builder) {
    this.day = builder.day;

  }

  public static OpeningSlotDTO.OpeningSlotDTOBuilder builder() {
    return new OpeningSlotDTO.OpeningSlotDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OpeningSlotDTOBuilder {

    private String day;

    public OpeningSlotDTO.OpeningSlotDTOBuilder day(String day) {
      this.day = day;
      return this;
    }

    public OpeningSlotDTO build() {
      OpeningSlotDTO openingSlotDTO = new OpeningSlotDTO(this);
      return openingSlotDTO;
    }
  }

  @Schema(name = "day", required = false)
  public String getDay() {
    return day;
  }
  public void setDay(String day) {
    this.day = day;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OpeningSlotDTO openingSlotDTO = (OpeningSlotDTO) o;
    return Objects.equals(this.day, openingSlotDTO.day);
  }

  @Override
  public int hashCode() {
    return Objects.hash(day);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OpeningSlotDTO{");
    sb.append(" day:").append(day);
    sb.append("}");
    return sb.toString();
  }


}
