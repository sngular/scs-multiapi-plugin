package com.sngular.scsplugin.modelclass.model.event.producer;

import com.sngular.scsplugin.modelclass.model.event.OrderDTO;

public interface IPublishOrder {

  OrderDTO publishOrder();
}