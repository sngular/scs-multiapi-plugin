package com.sngular.multifileplugin.testDateTime.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDate;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestDateDTO {

  @JsonProperty(value ="someDate")
  private LocalDate someDate;

  @JsonProperty(value ="someTime")
  private LocalDateTime someTime;


  @Builder
  @Jacksonized
  private TestDateDTO(LocalDate someDate, LocalDateTime someTime) {
    this.someDate = someDate;
    this.someTime = someTime;

  }

}
