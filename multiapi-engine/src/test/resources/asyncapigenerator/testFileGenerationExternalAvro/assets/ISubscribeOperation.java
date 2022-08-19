package net.coru.scsplugin.externalAvro.model.event.consumer;

import net.coru.scsplugin.externalAvro.model.event.CreateOrder;

public interface ISubscribeOperationExternalAvro {

    void subscribeOperationExternalAvro(CreateOrder value);
}