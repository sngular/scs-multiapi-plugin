package com.sngular.generator.multiapi.model.event.producer;

import com.sngular.generator.multiapi.model.event.messages.CreateOrderMapper;

public interface ISubscribeOperation {

  CreateOrderMapper subscribeOperation();
}