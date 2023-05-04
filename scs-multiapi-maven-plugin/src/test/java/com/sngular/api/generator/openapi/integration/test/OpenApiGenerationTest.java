/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.openapi.integration.test;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

import com.sngular.api.generator.test.utils.TestUtils;
import com.soebes.itf.jupiter.extension.MavenGoal;
import com.soebes.itf.jupiter.extension.MavenJupiterExtension;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.maven.MavenProjectResult;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

@MavenRepository
@MavenJupiterExtension
@Execution(ExecutionMode.SAME_THREAD)
public class OpenApiGenerationTest {

  @MavenTest
  @MavenGoal("generate-sources")
  void testApiMultiGeneration(MavenProjectResult result) throws IOException {
    List<String> expectedFileFirst = Collections.singletonList("com/sngular/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                               "/TestFirstApi.java");
    List<String> expectedFileSecond = Collections.singletonList("com/sngular/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                                "/TestSecondApi.java");

    List<String> expectedExceptionFilesFirst = Collections.singletonList(
        "com/sngular/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets/ModelClassExceptionFirst.java");

    List<String> expectedExceptionFilesSecond = Collections.singletonList(
        "com/sngular/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets/ModelClassExceptionSecond.java");

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
    List<String> expectedFiles = List.of("TestApi.java", "TestObj.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toAbsolutePath();
    File targetDirectory = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular").toFile();

    TestUtils.checkTargetFiles(expectedFiles, targetDirectory);
  }
}
