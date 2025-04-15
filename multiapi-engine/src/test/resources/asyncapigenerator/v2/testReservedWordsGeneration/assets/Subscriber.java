package com.sngular.scsplugin.reservedwordsgeneration.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.reservedwordsgeneration.model.event.CreateOrderDTO;

@Configuration
public class Subscriber {

  private final ISubscribeOperationFileGeneration subscribeOperationFileGeneration;

  protected Subscriber(final ISubscribeOperationFileGeneration subscribeOperationFileGeneration) {
    this.subscribeOperationFileGeneration = subscribeOperationFileGeneration;
  }

  @Bean
  public Consumer<CreateOrderDTO> subscribeOperationFileGeneration() {
    return value -> subscribeOperationFileGeneration.subscribeOperationFileGeneration(value);
  }


}
