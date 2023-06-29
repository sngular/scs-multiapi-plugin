package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer;

import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.messages.OrderCreatedDTO;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.MessageDTO;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer.MessageWrapper;

public interface IPublishOperationFileGenerationWithKafkaBindings {

  void publishOperationFileGenerationWithKafkaBindings(final MessageWrapper<OrderCreatedDTO, MessageDTO> value);
}