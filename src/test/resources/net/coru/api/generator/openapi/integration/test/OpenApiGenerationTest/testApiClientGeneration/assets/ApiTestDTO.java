package net.coru.multifileplugin.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import javax.validation.constraints.NotNull;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ApiTestDTO {

  @JsonProperty(value ="name")
  @NotNull
  private String name;

  @JsonProperty(value ="id")
  @NotNull
  private Integer id;

}
