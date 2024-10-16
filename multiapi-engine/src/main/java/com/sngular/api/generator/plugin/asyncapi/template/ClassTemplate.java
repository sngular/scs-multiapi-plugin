/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.template;

import java.nio.file.Path;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class ClassTemplate {

  Path filePath;

  Path propertiesPath;

  String modelPackage;

  String className;

  String keyClassName;

  SchemaObject classSchema;

  boolean useLombok;

}
