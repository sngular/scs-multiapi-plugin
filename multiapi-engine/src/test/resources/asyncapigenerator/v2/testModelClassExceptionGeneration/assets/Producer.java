package com.sngular.scsplugin.modelclass.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.modelclass.model.event.OrderDTO;

@Configuration
public class Producer {

  private final IPublishOrder publishOrder;

  protected Producer(final IPublishOrder publishOrder) {
    this.publishOrder = publishOrder;
  }

  @Bean
  public Supplier<OrderDTO> publishOrder() {
    return () -> publishOrder.publishOrder();
  }


}
