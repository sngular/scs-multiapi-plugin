package net.coru.multifileplugin.lombok.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
@Builder
public class ApiErrorDTO {

  @JsonProperty(value ="message")
  @NonNull
  private String message;

  @JsonProperty(value ="code")
  @NonNull
  private Integer code;

}
