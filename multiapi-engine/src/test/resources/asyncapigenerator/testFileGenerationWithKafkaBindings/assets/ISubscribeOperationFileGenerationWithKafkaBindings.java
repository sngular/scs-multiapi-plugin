package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer;

import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.CreateOrderMapper;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer.MessageWrapper;

public interface ISubscribeOperationFileGenerationWithKafkaBindings {

  MessageWrapper<CreateOrderMapper, String> subscribeOperationFileGenerationWithKafkaBindings();
}