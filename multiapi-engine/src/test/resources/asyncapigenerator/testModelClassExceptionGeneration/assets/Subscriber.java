package com.sngular.scsplugin.modelclass.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.modelclass.model.event.CreateOrderEventDTO;

@Configuration
public class Subscriber {

  private final ISubscribeOrder subscribeOrder;

  protected Subscriber(final ISubscribeOrder subscribeOrder) {
    this.subscribeOrder = subscribeOrder;
  }

  @Bean
  public Consumer<CreateOrderEventDTO> subscribeOrder() {
    return value -> subscribeOrder.subscribeOrder(value);
  }


}
