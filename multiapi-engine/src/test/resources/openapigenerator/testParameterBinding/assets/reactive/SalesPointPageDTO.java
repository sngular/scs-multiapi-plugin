package com.sngular.multifileplugin.parameterbindingreactive.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = SalesPointPageDTO.SalesPointPageDTOBuilder.class)
public class SalesPointPageDTO {

  @JsonProperty(value ="closing_days")
  private List<String> closing_days;

  private SalesPointPageDTO(SalesPointPageDTOBuilder builder) {
    this.closing_days = builder.closing_days;

  }

  public static SalesPointPageDTO.SalesPointPageDTOBuilder builder() {
    return new SalesPointPageDTO.SalesPointPageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SalesPointPageDTOBuilder {

    private List<String> closing_days = new ArrayList<String>();

    public SalesPointPageDTO.SalesPointPageDTOBuilder closing_days(List<String> closing_days) {
      if (Objects.nonNull(closing_days) && !closing_days.isEmpty()) {
        this.closing_days.addAll(closing_days);
      }
      return this;
    }

    public SalesPointPageDTO.SalesPointPageDTOBuilder closing_day(String closing_day) {
      if (Objects.nonNull(closing_day)) {
        this.closing_days.add(closing_day);
      }
      return this;
    }

    public SalesPointPageDTO build() {
      SalesPointPageDTO salesPointPageDTO = new SalesPointPageDTO(this);
      return salesPointPageDTO;
    }
  }

  @Schema(name = "closing_days", required = false)
  public List<String> getClosing_days() {
    return closing_days;
  }
  public void setClosing_days(List<String> closing_days) {
    this.closing_days = closing_days;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SalesPointPageDTO salesPointPageDTO = (SalesPointPageDTO) o;
    return Objects.equals(this.closing_days, salesPointPageDTO.closing_days);
  }

  @Override
  public int hashCode() {
    return Objects.hash(closing_days);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("SalesPointPageDTO{");
    sb.append(" closing_days:").append(closing_days);
    sb.append("}");
    return sb.toString();
  }


}
