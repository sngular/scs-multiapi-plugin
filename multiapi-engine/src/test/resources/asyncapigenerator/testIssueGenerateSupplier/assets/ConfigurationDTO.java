package company.mail.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ConfigurationDTO {

  @JsonProperty(value ="name")
  private String name;


  @Builder
  @Jacksonized
  private ConfigurationDTO(String name) {
    this.name = name;

  }

}
