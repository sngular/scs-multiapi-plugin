package com.sngular.apigenerator.asyncapi;

import com.sngular.apigenerator.asyncapi.model.messages.CreateOrder;

public interface ISubscribeOperation {

  void subscribeOperation(final CreateOrder value);
}