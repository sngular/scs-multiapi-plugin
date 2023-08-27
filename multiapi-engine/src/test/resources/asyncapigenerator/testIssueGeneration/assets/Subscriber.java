package com.sngular.scsplugin.issuegeneration.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.issuegeneration.model.event.schemas.StatusMsgDTO;

@Configuration
public class Subscriber {

  private final IResponse response;

  protected Subscriber(final IResponse response) {
    this.response = response;
  }

  @Bean
  public Consumer<StatusMsgDTO> response() {
    return value -> response.response(value);
  }


}
