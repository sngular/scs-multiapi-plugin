package com.sngular.multifileplugin.testDateTimeZoned.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.ZonedDateTime;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestDateDTO {

  @JsonProperty(value ="someDate")
  private ZonedDateTime someDate;

  @JsonProperty(value ="someTime")
  private ZonedDateTime someTime;


  @Builder
  @Jacksonized
  private TestDateDTO(ZonedDateTime someDate, ZonedDateTime someTime) {
    this.someDate = someDate;
    this.someTime = someTime;

  }

}
