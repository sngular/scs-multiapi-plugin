package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.messages.OrderCreatedDTO;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer.MessageWrapper;
import org.springframework.messaging.Message;

@Configuration
public class TestClassName {

  private final IPublishOperationFileGenerationWithKafkaBindings publishOperationFileGenerationWithKafkaBindings;

  protected TestClassName(final IPublishOperationFileGenerationWithKafkaBindings publishOperationFileGenerationWithKafkaBindings) {
    this.publishOperationFileGenerationWithKafkaBindings = publishOperationFileGenerationWithKafkaBindings;
  }

  @Bean
  public Consumer<Message<OrderCreatedDTO>> publishOperationFileGenerationWithKafkaBindings() {
    return value -> {
      final var messageWrapper = MessageWrapper.<OrderCreatedDTO, String>builder().payload(value.getPayload()).key(value.getKey()).build();
      publishOperationFileGenerationWithKafkaBindings.publishOperationFileGenerationWithKafkaBindings(messageWrapper);
    }
  }


}