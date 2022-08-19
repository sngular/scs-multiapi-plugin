package net.coru.scsplugin.withOutIds.model.event.consumer;

import net.coru.scsplugin.withOutIds.model.event.CreateOrderDTO;

public interface ISubscribeOperation {

    void subscribeOperation(CreateOrderDTO value);
}