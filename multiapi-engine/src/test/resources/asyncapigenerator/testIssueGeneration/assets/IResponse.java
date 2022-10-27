package net.coru.scsplugin.issuegeneration.model.event.consumer;

import net.coru.scsplugin.issuegeneration.model.event.StatusDTO;

public interface IResponse {

  void response(final StatusDTO value);
}