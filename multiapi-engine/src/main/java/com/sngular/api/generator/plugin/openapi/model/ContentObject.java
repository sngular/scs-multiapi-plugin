/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ContentObject {

  private String name;

  private SchemaFieldObjectType dataType;

  private String importName;

  private SchemaObject schemaObject;

  /**
   * Whether the media type streams a sequence of documents (such as {@code application/x-ndjson}). The data type is then the
   * type of each streamed item.
   */
  private boolean streaming;

}
