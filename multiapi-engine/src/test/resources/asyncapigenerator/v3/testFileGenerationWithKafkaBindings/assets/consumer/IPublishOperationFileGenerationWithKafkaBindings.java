package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer;

import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.OrderDTO;

public interface IPublishOperationFileGenerationWithKafkaBindings {

  void publishOperationFileGenerationWithKafkaBindings(final OrderDTO value);
}