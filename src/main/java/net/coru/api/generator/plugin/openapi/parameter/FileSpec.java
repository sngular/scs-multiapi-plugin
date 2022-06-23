/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.parameter;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FileSpec {

  private String filePath;

  private String apiPackage;

  private String modelPackage;

  private String modelNamePrefix;

  private String modelNameSuffix;

  private String clientPackage;

  private Boolean callMode = false;

  private Boolean useTagsGroup = false;

  private Boolean useLombokModelAnnotation = false;

  private Boolean isReactive = false;
}
