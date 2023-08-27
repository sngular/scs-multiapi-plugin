package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer;

import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.schemas.OrderDTO;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer.MessageWrapper;

public interface IPublishOperationFileGenerationWithKafkaBindings {

  void publishOperationFileGenerationWithKafkaBindings(final MessageWrapper<OrderDTO, String> value);
}