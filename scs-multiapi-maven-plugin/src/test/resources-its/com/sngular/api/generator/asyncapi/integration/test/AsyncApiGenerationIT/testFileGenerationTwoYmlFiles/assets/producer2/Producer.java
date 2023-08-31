package com.sngular.apigenerator.asyncapi.producer2;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.apigenerator.asyncapi.model.OrderDTO;

@Configuration
public class Producer {

  private final IPublishOperation2 publishOperation2;

  protected Producer(final IPublishOperation2 publishOperation2) {
    this.publishOperation2 = publishOperation2;
  }

  @Bean
  public Supplier<OrderDTO> publishOperation2() {
    return () -> publishOperation2.publishOperation2();
  }


}
