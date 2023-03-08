package com.sngular.scsplugin.customvalidators.model.event.consumer;

import com.sngular.scsplugin.customvalidators.model.event.StatusDTO;

public interface ICustomValidatorResponse {

  void customValidatorResponse(final StatusDTO value);
}