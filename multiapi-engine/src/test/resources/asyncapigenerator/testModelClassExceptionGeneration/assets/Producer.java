package com.sngular.scsplugin.modelclass.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.modelclass.model.event.messages.OrderCreatedEventDTO;

@Configuration
public class Producer {

  private final IPublishOrder publishOrder;

  protected Producer(final IPublishOrder publishOrder) {
    this.publishOrder = publishOrder;
  }

  @Bean
  public Supplier<OrderCreatedEventDTO> publishOrder() {
    return () -> publishOrder.publishOrder();
  }


}
