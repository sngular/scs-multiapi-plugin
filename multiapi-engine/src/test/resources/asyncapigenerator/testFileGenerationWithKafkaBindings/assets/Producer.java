package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.schemas.CreateOrderMapper;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer.MessageWrapperMapper;

@Configuration
public class Producer {

  private final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings;

  private final com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.MessageDTO key;

  protected Producer(final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings) {
    this.subscribeOperationFileGenerationWithKafkaBindings = subscribeOperationFileGenerationWithKafkaBindings;
  }

  @Bean
  public Supplier<Message<CreateOrderMapper>> subscribeOperationFileGenerationWithKafkaBindings() {
    final var messageWrapper =  subscribeOperationFileGenerationWithKafkaBindings.subscribeOperationFileGenerationWithKafkaBindings();
    return () -> Message.build().payload((CreateOrder) messageWrapper.getPayload()).setHeader("key", (com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.MessageDTO) messageWrapper.getKey()).build();
  }


}
