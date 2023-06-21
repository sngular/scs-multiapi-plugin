package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer;

import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.messages.OrderCreatedDTO;

public interface IPublishOperationFileGenerationWithKafkaBindings {

  void publishOperationFileGenerationWithKafkaBindings(final OrderCreatedDTO value);
}