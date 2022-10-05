/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.template;

import java.nio.file.Path;

import lombok.Builder;
import lombok.Value;
import net.coru.api.generator.plugin.asyncapi.model.SchemaObject;

@Value
@Builder
public class ClassTemplate {

  Path filePath;

  String modelPackage;

  String className;

  SchemaObject classSchema;

}
