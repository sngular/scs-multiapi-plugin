package com.sngular.scsplugin.externalavro.model.event.consumer;

import com.sngular.testshop.commons.Receipt;

public interface ISubscribeReceiptExternalAvro {

  void subscribeReceiptExternalAvro(final Receipt value);
}