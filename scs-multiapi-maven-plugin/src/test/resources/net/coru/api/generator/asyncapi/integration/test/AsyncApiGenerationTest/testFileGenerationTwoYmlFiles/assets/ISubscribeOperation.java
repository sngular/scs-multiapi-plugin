package net.coru;

import net.coru.apigenerator.asyncapi.model.CreateOrder;

public interface ISubscribeOperation {

  void subscribeOperation(final CreateOrder value);
}