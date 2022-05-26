package com.corunet.scsplugin.business_model.model.event.producer;

import com.corunet.scsplugin.business_model.model.event.CreateOrderMapper;

public interface ISubscribeOperation {

    CreateOrderMapper subscribeOperation();
}