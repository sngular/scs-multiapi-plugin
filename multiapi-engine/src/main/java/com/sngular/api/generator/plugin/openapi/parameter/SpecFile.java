/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.parameter;

import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import lombok.*;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class SpecFile extends CommonSpecFile {

  private String clientPackage;

  private boolean callMode;

  private boolean useTagsGroup;

  private boolean isReactive;
}

