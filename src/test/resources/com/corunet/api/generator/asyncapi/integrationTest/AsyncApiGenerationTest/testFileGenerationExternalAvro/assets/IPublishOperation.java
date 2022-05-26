package com.corunet.scsplugin.business_model.model.event.producer;

import com.corunet.scsplugin.business_model.model.event.Order;

public interface IPublishOperation {

    Order publishOperation();
}