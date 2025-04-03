package com.sngular.scsplugin.reservedwordsgeneration.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.reservedwordsgeneration.model.event.OrderDTO;

@Configuration
public class Producer {

  private final IPublishOperationFileGeneration publishOperationFileGeneration;

  protected Producer(final IPublishOperationFileGeneration publishOperationFileGeneration) {
    this.publishOperationFileGeneration = publishOperationFileGeneration;
  }

  @Bean
  public Supplier<OrderDTO> publishOperationFileGeneration() {
    return () -> publishOperationFileGeneration.publishOperationFileGeneration();
  }


}
