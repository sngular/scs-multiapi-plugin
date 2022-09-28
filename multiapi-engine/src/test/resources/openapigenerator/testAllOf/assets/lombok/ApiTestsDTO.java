package net.coru.multifileplugin.lombok.testapi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;


@Data
public class ApiTestsDTO {

  @JsonProperty(value ="apiTestDTO")
  private ApiTestDTO apiTestDTO;


  @Builder
  private ApiTestsDTO(ApiTestDTO apiTestDTO) {
    this.apiTestDTO = apiTestDTO;

  }

}
