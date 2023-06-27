package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer;

import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.schemas.CreateOrderMapper;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer.MessageWrapperMapper;

public interface ISubscribeOperationFileGenerationWithKafkaBindings {

  MessageWrapperMapper subscribeOperationFileGenerationWithKafkaBindings(String key);
}