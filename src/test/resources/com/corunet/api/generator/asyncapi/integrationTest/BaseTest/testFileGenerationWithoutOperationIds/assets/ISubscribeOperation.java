package com.corunet.scsplugin.business_model.model.event.consumer;

import com.corunet.scsplugin.business_model.model.event.CreateOrderDTO;

public interface ISubscribeOperation {

    void subscribeOperation(CreateOrderDTO value);
}