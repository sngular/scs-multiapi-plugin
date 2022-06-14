package net.coru.multifileplugin.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ApiTestDTO {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="id")
  private Integer id;

}
