/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.template;

import com.sngular.api.generator.plugin.common.template.CommonTemplateIndexConstants;

public class TemplateIndexConstants extends CommonTemplateIndexConstants {

  public static final String TEMPLATE_API_SUPPLIERS = "templateSuppliers.ftlh";

  public static final String TEMPLATE_API_CONSUMERS = "templateConsumers.ftlh";

  public static final String TEMPLATE_API_STREAM_BRIDGE = "templateStreamBridge.ftlh";

  public static final String TEMPLATE_INTERFACE_SUPPLIERS = "interfaceSupplier.ftlh";

  public static final String TEMPLATE_INTERFACE_CONSUMERS = "interfaceConsumer.ftlh";

  public static final String TEMPLATE_MESSAGE_WRAPPER = "templateMessageWrapper.ftlh";

  public static final String KAFKA_BINDINGS_FTLH = "WithKafkaBindings.ftlh";

  protected TemplateIndexConstants() {
  }
}
