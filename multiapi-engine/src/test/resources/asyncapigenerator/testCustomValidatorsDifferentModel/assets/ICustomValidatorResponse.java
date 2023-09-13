package com.sngular.scsplugin.customvalidatordifferentmodel.event.consumer;

import com.sngular.scsplugin.customvalidatordifferentmodel.event.consumer.model.StatusMsgDTO;

public interface ICustomValidatorResponse {

  void customValidatorResponse(final StatusMsgDTO value);
}