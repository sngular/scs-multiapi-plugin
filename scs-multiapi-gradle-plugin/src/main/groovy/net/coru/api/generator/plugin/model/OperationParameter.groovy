/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.model

class OperationParameter {

  String ids

  String apiPackage

  String modelPackage

  String modelNameSuffix

  String classNamePostfix

  String getIds() {
    return ids
  }

  void setIds(final String ids) {
    this.ids = ids
  }

  String getApiPackage() {
    return apiPackage
  }

  void setApiPackage(final String apiPackage) {
    this.apiPackage = apiPackage
  }

  String getModelPackage() {
    return modelPackage
  }

  void setModelPackage(final String modelPackage) {
    this.modelPackage = modelPackage
  }

  String getModelNameSuffix() {
    return modelNameSuffix
  }

  void setModelNameSuffix(final String modelNameSuffix) {
    this.modelNameSuffix = modelNameSuffix
  }

  String getClassNamePostfix() {
    return classNamePostfix
  }

  void setClassNamePostfix(final String classNamePostfix) {
    this.classNamePostfix = classNamePostfix
  }

}