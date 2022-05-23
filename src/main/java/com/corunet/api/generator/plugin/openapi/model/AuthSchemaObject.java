package com.corunet.api.generator.plugin.openapi.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AuthSchemaObject {

  private String type;

  private String name;

  private String bearerSchema;

  private String apiKeyPlace;

  private String apiKeyParam;



}
