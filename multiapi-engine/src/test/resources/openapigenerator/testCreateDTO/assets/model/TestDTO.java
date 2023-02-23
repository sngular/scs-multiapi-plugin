package com.sngular.multifileplugin.testCreateDto.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestDTO {

  @JsonProperty(value ="properties")
  private List<PropertiesDTO> properties = new ArrayList<PropertiesDTO>();

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
