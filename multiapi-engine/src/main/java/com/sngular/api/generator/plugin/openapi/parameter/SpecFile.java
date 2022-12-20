/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.parameter;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SpecFile {

  private String filePath;

  private String apiPackage;

  private String modelPackage;

  private String modelNamePrefix;

  private String modelNameSuffix;

  private String clientPackage;

  private boolean callMode;

  private boolean useTagsGroup;

  private boolean useLombokModelAnnotation;

  private boolean isReactive;
}
