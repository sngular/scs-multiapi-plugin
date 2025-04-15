package com.sngular.scsplugin.issuesimpletypegeneration.model.event.consumer;

import com.sngular.scsplugin.issuesimpletypegeneration.model.event.StatusMsgDTO;

public interface IResponse {

  void response(final StatusMsgDTO value);
}