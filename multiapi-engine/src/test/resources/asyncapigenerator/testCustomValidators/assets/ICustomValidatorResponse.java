package com.sngular.scsplugin.customvalidator.model.event.consumer;

import com.sngular.scsplugin.customvalidator.model.event.schemas.StatusMsgDTO;

public interface ICustomValidatorResponse {

  void customValidatorResponse(final StatusMsgDTO value);
}