package com.sngular.scsplugin.filegenerationissue.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO;

@Configuration
public class TestClassName {

  private final IOnCustomerEvent onCustomerEvent;

  protected TestClassName(final IOnCustomerEvent onCustomerEvent) {
    this.onCustomerEvent = onCustomerEvent;
  }

  @Bean
  public Consumer<CustomerEventPayloadDTO> onCustomerEvent() {
    return value -> onCustomerEvent.onCustomerEvent(value);
  }


}
