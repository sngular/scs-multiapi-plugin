package net.coru.scsplugin.business_model.model.event.consumer;

import net.coru.scsplugin.business_model.model.event.CreateOrderDTO;

public interface ISubscribeOperation {

    void subscribeOperation(CreateOrderDTO value);
}