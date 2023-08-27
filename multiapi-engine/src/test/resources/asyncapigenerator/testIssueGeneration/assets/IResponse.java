package com.sngular.scsplugin.issuegeneration.model.event.consumer;

import com.sngular.scsplugin.issuegeneration.model.event.schemas.StatusMsgDTO;

public interface IResponse {

  void response(final StatusMsgDTO value);
}