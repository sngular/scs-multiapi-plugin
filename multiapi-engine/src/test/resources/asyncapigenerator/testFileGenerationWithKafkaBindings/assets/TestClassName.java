package com.sngular.scsplugin.filegenerationwithkey.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.filegenerationwithkey.model.event.messages.OrderCreatedDTO;

@Configuration
public class TestClassName {

  private final IPublishOperationFileGenerationWithKey publishOperationFileGenerationWithKey;

  protected TestClassName(final IPublishOperationFileGenerationWithKey publishOperationFileGenerationWithKey) {
    this.publishOperationFileGenerationWithKey = publishOperationFileGenerationWithKey;
  }

  @Bean
  public Consumer<Message<OrderCreatedDTO>> publishOperationFileGenerationWithKey() {
    return value -> {
      final var messageWrapper = MessageWrapper.builder().payload(value.getPayload()).key(value.getKey()).build();
      publishOperationFileGenerationWithKey.publishOperationFileGenerationWithKey(messageWrapper);
    }
  }


}