package net.coru.multifileplugin.lombok.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class ApiErrorDTO {

  @JsonProperty(value ="code")
  @NonNull
  private Integer code;

  @JsonProperty(value ="message")
  @NonNull
  private String message;


  @Builder
  private ApiErrorDTO(@NonNull Integer code, @NonNull String message) {
    this.code = code;
    this.message = message;

  }

}
