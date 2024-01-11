package com.sngular.multifileplugin.testcreatebasicdto.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestDTO {

  @JsonProperty(value ="properties")
  @NonNull
  private String properties;

  @JsonProperty(value ="id")
  @NonNull
  private String id;

  @JsonProperty(value ="address")
  private AddressDTO address;

  @JsonProperty(value ="age")
  @NonNull
  private BigDecimal age;


  @Builder
  @Jacksonized
  private TestDTO(@NonNull String properties, @NonNull String id, AddressDTO address, @NonNull BigDecimal age) {
    this.properties = properties;
    this.id = id;
    this.address = address;
    this.age = age;

  }

}
