package com.sngular.multifileplugin.testCreateDTO.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.Value;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestDTO {

  @JsonProperty(value ="properties")
  @Singular("property")
  private List<PropertiesDTO> properties;

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
  private TestDTO(List<PropertiesDTO> properties, @NonNull String id, AddressDTO address, @NonNull BigDecimal age) {
    this.properties = properties;
    this.id = id;
    this.address = address;
    this.age = age;

  }

}
