/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.openapi.integration.test;

import com.sngular.api.generator.test.utils.TestUtils;
import com.soebes.itf.jupiter.extension.MavenGoal;
import com.soebes.itf.jupiter.extension.MavenJupiterExtension;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.maven.MavenProjectResult;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
import static java.util.Collections.singletonList;

@MavenRepository
@MavenJupiterExtension
@Execution(ExecutionMode.SAME_THREAD)
public class OpenApiGenerationIT {

  @MavenTest
  @MavenGoal("generate-sources")
  void testApiMultiGeneration(MavenProjectResult result) throws IOException {
    List<String> expectedFileFirst = singletonList(
      "com/sngular/api/generator/openapi/integration/test/OpenApiGenerationIT/testApiMultiGeneration/assets/TestFirstApi.java");
    List<String> expectedFileSecond = singletonList(
      "com/sngular/api/generator/openapi/integration/test/OpenApiGenerationIT/testApiMultiGeneration/assets/TestSecondApi.java");

    List<String> expectedExceptionFilesFirst = singletonList(
      "com/sngular/api/generator/openapi/integration/test/OpenApiGenerationIT/testApiMultiGeneration/assets/ModelClassExceptionFirst.java");

    List<String> expectedExceptionFilesSecond = singletonList(
      "com/sngular/api/generator/openapi/integration/test/OpenApiGenerationIT/testApiMultiGeneration/assets/ModelClassExceptionSecond.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toAbsolutePath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/multifileplugin/testmultifile/first");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/multifileplugin/testmultifile/second");
    Path pathToExceptionFirst = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/multifileplugin/testmultifile/first/model/exception");
    Path pathToExceptionSecond = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/multifileplugin/testmultifile/second/model/exception");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    File targetSecondFolder = pathToTargetSecond.toFile();
    assertThat(targetSecondFolder).isNotEmptyDirectory();

    File targetExceptionFirstFolder = pathToExceptionFirst.toFile();
    assertThat(targetExceptionFirstFolder).isNotEmptyDirectory();

    File targetExceptionSecondFolder = pathToExceptionSecond.toFile();
    assertThat(targetExceptionSecondFolder).isNotEmptyDirectory();

    TestUtils.validateFiles(expectedFileFirst, targetFirstFolder);
    TestUtils.validateFiles(expectedFileSecond, targetSecondFolder);
    TestUtils.validateFiles(expectedExceptionFilesFirst, targetExceptionFirstFolder);
    TestUtils.validateFiles(expectedExceptionFilesSecond, targetExceptionSecondFolder);
  }

  @MavenTest
  @MavenGoal("generate-sources")
  void testDependencyYml(MavenProjectResult result) throws IOException {
    List<String> expectedModel = singletonList("TestObj.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toAbsolutePath();
    File targetModelDirectory = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/apigenerator/openapi").toFile();

    TestUtils.checkTargetFiles(expectedModel, targetModelDirectory);

    List<String> expectedApi = singletonList("TestApi.java");
    File targetApiDirectory = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/api").toFile();
    TestUtils.checkTargetFiles(expectedApi, targetApiDirectory);

  }
}
