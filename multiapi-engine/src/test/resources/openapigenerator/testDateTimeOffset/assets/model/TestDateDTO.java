package com.sngular.multifileplugin.testDateTimeOffset.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.OffsetDateTime;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestDateDTO {

  @JsonProperty(value ="someDate")
  private OffsetDateTime someDate;

  @JsonProperty(value ="someTime")
  private OffsetDateTime someTime;


  @Builder
  @Jacksonized
  private TestDateDTO(OffsetDateTime someDate, OffsetDateTime someTime) {
    this.someDate = someDate;
    this.someTime = someTime;

  }

}
