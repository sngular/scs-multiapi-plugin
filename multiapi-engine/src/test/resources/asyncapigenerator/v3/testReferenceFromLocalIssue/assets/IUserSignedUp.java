package com.github.issue.listener;

import com.github.issue.model.UserMessage;

public interface IUserSignedUp {

  void userSignedUp(final UserMessage value);
}