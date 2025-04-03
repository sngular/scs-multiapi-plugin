package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.CreateOrderMapper;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer.MessageWrapper;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;

@Configuration
public class Producer {

  private final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings;

  protected Producer(final ISubscribeOperationFileGenerationWithKafkaBindings subscribeOperationFileGenerationWithKafkaBindings) {
    this.subscribeOperationFileGenerationWithKafkaBindings = subscribeOperationFileGenerationWithKafkaBindings;
  }

  @Bean
  public Supplier<Message<CreateOrderMapper>> subscribeOperationFileGenerationWithKafkaBindings() {
    final var messageWrapper =  subscribeOperationFileGenerationWithKafkaBindings.subscribeOperationFileGenerationWithKafkaBindings();
    return () -> MessageBuilder.withPayload((CreateOrderMapper) messageWrapper.getPayload()).setHeader(KafkaHeaders.MESSAGE_KEY, (String) messageWrapper.getKey()).build();
  }


}
