package com.sngular.scsplugin.fileGenerationWithKafkaBindings.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.fileGenerationWithKafkaBindings.model.event.schemas.CreateOrderMapper;

@Configuration
public class Producer {

  private final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings;

  protected Producer(final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings) {
    this.subscribeOperationFileGenerationWithKafkaBindings = subscribeOperationFileGenerationWithKafkaBindings;
  }

  @Bean
  public Supplier<CreateOrderMapper> subscribeOperationFileGenerationWithKafkaBindings() {
    return () -> subscribeOperationFileGenerationWithKafkaBindings.subscribeOperationFileGenerationWithKafkaBindings();
  }


}
