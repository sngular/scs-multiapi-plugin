package com.sngular.scsplugin.customvalidator.model.event.consumer;

import com.sngular.scsplugin.customvalidator.model.event.StatusDTO;

public interface ICustomValidatorResponse {

  void customValidatorResponse(final StatusDTO value);
}