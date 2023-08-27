package com.sngular.scsplugin.modelclass.model.event.producer;

import com.sngular.scsplugin.modelclass.model.event.schemas.OrderDTO;

public interface IPublishOrder {

  OrderDTO publishOrder();
}