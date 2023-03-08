package com.sngular.scsplugin.issuegeneration.model.event.consumer;

import com.sngular.scsplugin.issuegeneration.model.event.StatusDTO;

public interface IResponse {

  void response(final StatusDTO value);
}