package com.sngular.apigenerator.asyncapi;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.apigenerator.asyncapi.model.CreateOrder;

@Configuration
public class Subscriber {

  private final ISubscribeOperation subscribeOperation;

  protected Subscriber(final ISubscribeOperation subscribeOperation) {
    this.subscribeOperation = subscribeOperation;
  }

  @Bean
  public Consumer<CreateOrder> subscribeOperation() {
    return value -> subscribeOperation.subscribeOperation(value);
  }


}
