package com.sngular.scsplugin.modelclass.model.event.producer;

import com.sngular.scsplugin.modelclass.model.event.messages.OrderCreatedEventDTO;

public interface IPublishOrder {

  OrderCreatedEventDTO publishOrder();
}