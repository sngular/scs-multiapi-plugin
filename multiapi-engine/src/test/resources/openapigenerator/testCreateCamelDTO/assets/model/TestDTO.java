package com.sngular.multifileplugin.testCreateCamelDTO.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestDTO {

  @JsonProperty(value ="contract_age")
  @NonNull
  private BigDecimal contractAge;

  @JsonProperty(value ="contract_properties")
  @Singular("contractProperty")
  private List<ContractPropertiesDTO> contractProperties;

  @JsonProperty(value ="id")
  @NonNull
  private String id;

  @JsonProperty(value ="address")
  private AddressDTO address;


  @Builder
  @Jacksonized
  private TestDTO(@NonNull BigDecimal contractAge, List<ContractPropertiesDTO> contractProperties, @NonNull String id, AddressDTO address) {
    this.contractAge = contractAge;
    this.contractProperties = contractProperties;
    this.id = id;
    this.address = address;

  }

}
