package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer;

import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.messages.OrderCreatedDTO;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer.MessageWrapperDTO;

public interface IPublishOperationFileGenerationWithKafkaBindings {

  void publishOperationFileGenerationWithKafkaBindings(final MessageWrapperDTO value);
}