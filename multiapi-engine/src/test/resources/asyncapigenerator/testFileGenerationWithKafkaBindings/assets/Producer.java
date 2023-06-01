package com.sngular.scsplugin.filegenerationwithkey.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationwithkey.model.event.schemas.CreateOrderMapper;

@Configuration
public class Producer {

  private final ISubscribeOperationFileGenerationWithKey subscribeOperationFileGenerationWithKey;

  protected Producer(final ISubscribeOperationFileGenerationWithKey subscribeOperationFileGenerationWithKey) {
    this.subscribeOperationFileGenerationWithKey = subscribeOperationFileGenerationWithKey;
  }

  @Bean
  public Supplier<CreateOrderMapper> subscribeOperationFileGenerationWithKey() {
    return () -> subscribeOperationFileGenerationWithKey.subscribeOperationFileGenerationWithKey();
  }


}
