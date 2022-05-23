package com.corunet.scsplugin.business_model.model.event.consumer;

import com.corunet.scsplugin.business_model.model.event.OrderCreatedDTO;

public interface IPublishOperation {

    void publishOperation(OrderCreatedDTO value);
}