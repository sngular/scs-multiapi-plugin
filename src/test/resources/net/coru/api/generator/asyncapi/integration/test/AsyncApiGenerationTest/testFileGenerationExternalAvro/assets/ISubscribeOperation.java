package net.coru.scsplugin.business_model.model.event.consumer;

import net.coru.scsplugin.business_model.model.event.CreateOrder;

public interface ISubscribeOperation {

    void subscribeOperation(CreateOrder value);
}