package com.sngular.scsplugin.filegeneration.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegeneration.model.event.OrderCreatedDTO;

@Configuration
public class TestClassName {

  private final IPublishOperationFileGeneration publishOperationFileGeneration;

  protected TestClassName(final IPublishOperationFileGeneration publishOperationFileGeneration) {
    this.publishOperationFileGeneration = publishOperationFileGeneration;
  }

  @Bean
  public Consumer<OrderCreatedDTO> publishOperationFileGeneration() {
    return value -> publishOperationFileGeneration.publishOperationFileGeneration(value);
  }


}