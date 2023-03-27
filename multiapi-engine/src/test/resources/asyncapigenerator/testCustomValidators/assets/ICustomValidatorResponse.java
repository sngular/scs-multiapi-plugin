package com.sngular.scsplugin.customvalidator.model.event.consumer;

import com.sngular.scsplugin.customvalidator.model.event.messages.StatusDTO;

public interface ICustomValidatorResponse {

  void customValidatorResponse(final StatusDTO value);
}