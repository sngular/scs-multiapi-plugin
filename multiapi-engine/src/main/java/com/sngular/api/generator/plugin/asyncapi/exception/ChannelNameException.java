/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.exception;

public class ChannelNameException extends RuntimeException {

  private static final String ERROR_MESSAGE = "AsyncApi -> Channel name %s includes a separator not allowed. Please use \".\" or \"-\" instead.";

  public ChannelNameException(final String channelName) {
    super(String.format(ERROR_MESSAGE, channelName));
  }
}
