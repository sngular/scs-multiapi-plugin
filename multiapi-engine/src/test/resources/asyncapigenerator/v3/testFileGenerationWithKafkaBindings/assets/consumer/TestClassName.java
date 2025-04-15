package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.OrderDTO;

@Configuration
public class TestClassName {

  private final IPublishOperationFileGenerationWithKafkaBindings publishOperationFileGenerationWithKafkaBindings;

  protected TestClassName(final IPublishOperationFileGenerationWithKafkaBindings publishOperationFileGenerationWithKafkaBindings) {
    this.publishOperationFileGenerationWithKafkaBindings = publishOperationFileGenerationWithKafkaBindings;
  }

  @Bean
  public Consumer<OrderDTO> publishOperationFileGenerationWithKafkaBindings() {
    return value -> publishOperationFileGenerationWithKafkaBindings.publishOperationFileGenerationWithKafkaBindings(value);
  }


}
