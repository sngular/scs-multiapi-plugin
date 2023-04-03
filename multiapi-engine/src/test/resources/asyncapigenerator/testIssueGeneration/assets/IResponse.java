package com.sngular.scsplugin.issuegeneration.model.event.consumer;

import com.sngular.scsplugin.issuegeneration.model.event.messages.StatusDTO;

public interface IResponse {

  void response(final StatusDTO value);
}