package net.coru.multifileplugin.enumlombokgeneration.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;


@Data
public class ApiTestsDTO {

  @JsonProperty(value ="ApiTestDTO")
  private ApiTestDTO ApiTestDTO;


  @Builder
  private ApiTestsDTO(ApiTestDTO ApiTestDTO){
    this.ApiTestDTO = ApiTestDTO;

  }

}
