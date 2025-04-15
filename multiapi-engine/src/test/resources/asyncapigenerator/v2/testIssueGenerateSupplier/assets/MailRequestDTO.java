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

  @JsonProperty(value ="configuration")
  private ConfigurationDTO configuration;

  @JsonProperty(value ="recipients")
  @Singular("recipient")
  private List<String> recipients;


  @Builder
  @Jacksonized
  private MailRequestDTO(String sender, ConfigurationDTO configuration, List<String> recipients) {
    this.sender = sender;
    this.configuration = configuration;
    this.recipients = recipients;

  }

}
