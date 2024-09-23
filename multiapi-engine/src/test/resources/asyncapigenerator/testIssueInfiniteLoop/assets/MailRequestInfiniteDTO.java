package com.sngular.scsplugin.infiniteLoop.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class MailRequestInfiniteDTO {

  @JsonProperty(value ="sender")
  private String sender;

  @JsonProperty(value ="config")
  private ConfigDTO config;

  @JsonProperty(value ="recipients")
  @Singular("recipient")
  private List<String> recipients;


  @Builder
  @Jacksonized
  private MailRequestInfiniteDTO(String sender, ConfigDTO config, List<String> recipients) {
    this.sender = sender;
    this.config = config;
    this.recipients = recipients;

  }

}
