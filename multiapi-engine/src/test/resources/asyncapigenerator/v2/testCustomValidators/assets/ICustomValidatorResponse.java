package com.sngular.scsplugin.customvalidator.model.event.consumer;

import com.sngular.scsplugin.customvalidator.model.event.StatusMsgDTO;

public interface ICustomValidatorResponse {

  void customValidatorResponse(final StatusMsgDTO value);
}