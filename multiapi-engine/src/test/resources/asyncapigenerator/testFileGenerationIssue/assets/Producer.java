package com.sngular.scsplugin.filegenerationissue.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationissue.model.event.CustomerOrderEventMessageDTO;

@Configuration
public class Producer {

  private final IOnCustomerOrderEvent onCustomerOrderEvent;

  protected Producer(final IOnCustomerOrderEvent onCustomerOrderEvent) {
    this.onCustomerOrderEvent = onCustomerOrderEvent;
  }

  @Bean
  public Supplier<CustomerOrderEventMessageDTO> onCustomerOrderEvent() {
    return () -> onCustomerOrderEvent.onCustomerOrderEvent();
  }


}
