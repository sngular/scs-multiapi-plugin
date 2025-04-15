package com.sngular.scsplugin.issuesimpletypegeneration.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.issuesimpletypegeneration.model.event.StatusMsgDTO;

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
