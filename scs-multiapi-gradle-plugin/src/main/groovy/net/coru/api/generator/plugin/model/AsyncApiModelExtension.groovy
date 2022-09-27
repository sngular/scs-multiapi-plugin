/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
package net.coru.api.generator.plugin.model

import org.gradle.api.Action
import org.gradle.util.internal.ConfigureUtil

class AsyncApiModelExtension {

  List<AsyncApiSpecFile> specFile = new ArrayList<>()

  Boolean overWriteModel = Boolean.FALSE

  List<AsyncApiSpecFile> getSpecFile() {
    return specFile
  }

  Boolean getOverWriteModel() {
    return overWriteModel
  }

  void specFile(Closure configuration) {
    AsyncApiSpecFile apiSpecFile = new AsyncApiSpecFile()
    ConfigureUtil.configure(configuration, apiSpecFile)
    specFile.add(apiSpecFile)
  }

  void specFile(Action<? super AsyncApiSpecFile> configuration) {
    AsyncApiSpecFile apiSpecFile = new AsyncApiSpecFile()
    configuration.execute(apiSpecFile)
    specFile.add(apiSpecFile)
  }
}
