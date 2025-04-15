package com.sngular.scsplugin.notgeneratedproperties.consumer;

import com.sngular.scsplugin.model.schemas.UserSignedUp;

public interface IEmitUserSignUpEvent {

  void emitUserSignUpEvent(final UserSignedUp value);
}