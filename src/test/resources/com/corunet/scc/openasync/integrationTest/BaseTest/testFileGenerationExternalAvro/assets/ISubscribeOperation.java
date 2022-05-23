package com.corunet.scsplugin.business_model.model.event.consumer;

import com.corunet.scsplugin.business_model.model.event.CreateOrder;

public interface ISubscribeOperation {

    void subscribeOperation(CreateOrder value);
}