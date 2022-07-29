/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.model;

import java.util.List;

abstract class OperationParameterObject {

  abstract String ids();

  abstract String apiPackage();

  abstract String modelPackage();

  abstract String modelNameSuffix();

  abstract String classNamePostfix();

  abstract List<String> operationIds();

}
