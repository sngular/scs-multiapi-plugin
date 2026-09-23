package com.sngular.multifileplugin.testnestedrefinallof.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class InlineResponse200ListProductsDTO {

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="id")
  private String id;

  @JsonProperty(value ="sku")
  private String sku;

  @JsonProperty(value ="price")
  private Double price;


  @Builder
  @Jacksonized
  private InlineResponse200ListProductsDTO(String name, String id, String sku, Double price) {
    this.name = name;
    this.id = id;
    this.sku = sku;
    this.price = price;

  }

}
