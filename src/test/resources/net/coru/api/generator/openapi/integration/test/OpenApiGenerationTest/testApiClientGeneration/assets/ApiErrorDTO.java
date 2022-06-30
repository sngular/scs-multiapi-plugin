package net.coru.multifileplugin.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import javax.validation.constraints.NotNull;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ApiErrorDTO {

  @JsonProperty(value ="message")
  @NotNull
  private String message;
  @JsonProperty(value ="code")
  @NotNull
  private Integer code;

}
