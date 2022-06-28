package net.coru.multifileplugin.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ApiErrorDTO {

  @JsonProperty(value ="message")
  private String message;

  @JsonProperty(value ="code")
  private Integer code;

}
