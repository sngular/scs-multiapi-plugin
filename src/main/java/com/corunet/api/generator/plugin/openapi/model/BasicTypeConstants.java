/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.openapi.model;

import java.util.Set;

public class BasicTypeConstants {

  public static final String NUMBER = "number";

  public static final String STRING = "string";

  public static final String BOOLEAN = "boolean";

  public static final String INTEGER = "integer";

  public static final Set<String> BASIC_OBJECT_TYPE = Set.of(NUMBER, STRING, BOOLEAN, INTEGER);

  private BasicTypeConstants() {

  }

}
