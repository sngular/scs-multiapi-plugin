package net.coru.multifileplugin.lombok.testapi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;


@Data
public class ApiErrorDTO {

  @JsonProperty(value ="message")
  @NonNull
  private String message;

  @JsonProperty(value ="code")
  @NonNull
  private Integer code;


  @Builder
  private ApiErrorDTO(@NonNull String message, @NonNull Integer code){
    this.message = message;
    this.code = code;

  }

}
