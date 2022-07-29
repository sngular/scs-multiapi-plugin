package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiClientGeneration.assets;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;


@Data
public class ApiTestDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private Integer id;


  @Builder
  private ApiTestDTO(@NonNull String name, @NonNull Integer id){
    this.name = name;
    this.id = id;

  }

}
