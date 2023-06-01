package com.sngular.scsplugin.filegenerationwithkey.model.event.producer;

import com.sngular.scsplugin.filegenerationwithkey.model.event.schemas.CreateOrderMapper;

public interface ISubscribeOperationFileGenerationWithKey {

  CreateOrderMapper subscribeOperationFileGenerationWithKey(String key);
}