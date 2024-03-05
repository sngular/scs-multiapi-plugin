/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.util;

public class MapperUtil {

  public static final String INTEGER = "integer";
  
  public static final String FLOAT = "float";



  private static final String SLASH = "/";

  private MapperUtil() {}
  

  public static String buildKey(final String[] pathList) {
    final var arrayLength = pathList.length;
    return (arrayLength > 2 ? pathList[arrayLength - 2] + SLASH + pathList[arrayLength - 1] : pathList[0]).toUpperCase();
  }

}
