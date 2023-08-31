package com.sngular.apigenerator.asyncapi;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.apigenerator.asyncapi.model.Order;

@Configuration
public class Producer {

  private final IPublishOperation publishOperation;

  protected Producer(final IPublishOperation publishOperation) {
    this.publishOperation = publishOperation;
  }

  @Bean
  public Supplier<Order> publishOperation() {
    return () -> publishOperation.publishOperation();
  }


}
