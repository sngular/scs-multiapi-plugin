/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.exception;

public class KafkaTopicSeparatorException extends RuntimeException {

  public KafkaTopicSeparatorException(final String channelName) {
    super("Channel name " + channelName + " includes a separator not allowed. Please use \".\" or \"-\" instead.");
  }
}
