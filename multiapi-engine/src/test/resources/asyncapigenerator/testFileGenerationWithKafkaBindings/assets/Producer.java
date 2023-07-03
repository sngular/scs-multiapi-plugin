package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.schemas.CreateOrderMapper;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer.MessageWrapper;

@Configuration
public class Producer {

  private final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings;

  protected Producer(final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings) {
    this.subscribeOperationFileGenerationWithKafkaBindings = subscribeOperationFileGenerationWithKafkaBindings;
  }

  @Bean
  public Supplier<Message<CreateOrderMapper>> subscribeOperationFileGenerationWithKafkaBindings() {
    final var messageWrapper =  subscribeOperationFileGenerationWithKafkaBindings.subscribeOperationFileGenerationWithKafkaBindings();
    return () -> Message.build().payload((CreateOrderMapper) messageWrapper.getPayload()).setHeader("key", (String) messageWrapper.getKey()).build();
  }


}
