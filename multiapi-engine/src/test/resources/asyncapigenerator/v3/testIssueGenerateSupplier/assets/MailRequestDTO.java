package company.mail.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class MailRequestDTO {

  @JsonProperty(value ="sender")
  private String sender;

  @JsonProperty(value ="config")
  private ConfigurationDTO config;

  @JsonProperty(value ="recipients")
  @Singular(value = "recipient", ignoreNullCollections = true)
  private List<String> recipients;


  @Builder
  @Jacksonized
  private MailRequestDTO(String sender, ConfigurationDTO config, List<String> recipients) {
    this.sender = sender;
    this.config = config;
    this.recipients = recipients;

  }

}
