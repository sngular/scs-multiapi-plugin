package net.coru.scsplugin.business_model.model.event.producer;

import net.coru.scsplugin.business_model.model.event.CreateOrderMapper;

public interface ISubscribeOperation {

    CreateOrderMapper subscribeOperation();
}