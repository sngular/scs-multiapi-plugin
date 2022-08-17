/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.openapi.integration.test;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
import static net.coru.api.generator.test.utils.TestUtils.validateFiles;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.soebes.itf.jupiter.extension.MavenGoal;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.maven.MavenProjectResult;

public class OpenApiGenerationTest {

  private final String DEFAULT_TARGET_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi";

  private final String DEFAULT_MODEL_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi/model";

  private final String DEFAULT_EXCEPTION_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi/model/exception";


  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiEnumsGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ApiTestInfoDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ApiTestsDTO.java")
    );

    List<File> expectedExceptionFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ModelClassException.java"));

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiEnumsLombokGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/ApiTestInfoDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/ApiTestsDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, null, null);
  }

/*
  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiMultiGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                    "/TestFirstApi.java"));
    List<File> expectedFileSecond = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                     "/TestSecondApi.java"));

    List<File> expectedExceptionFilesFirst = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets/ModelClassExceptionFirst.java"));

    List<File> expectedExceptionFilesSecond = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets/ModelClassExceptionSecond.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/first");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/second");
    Path pathToExceptionFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/first/model/exception");
    Path pathToExceptionSecond = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/second/model/exception");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    File targetSecondFolder = pathToTargetSecond.toFile();
    assertThat(targetSecondFolder).isNotEmptyDirectory();

    File targetExceptionFirstFolder = pathToExceptionFirst.toFile();
    assertThat(targetExceptionFirstFolder).isNotEmptyDirectory();

    File targetExceptionSecondFolder = pathToExceptionSecond.toFile();
    assertThat(targetExceptionSecondFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
    validateFiles(expectedFileSecond, targetSecondFolder);
    validateFiles(expectedExceptionFilesFirst, targetExceptionFirstFolder);
    validateFiles(expectedExceptionFilesSecond, targetExceptionSecondFolder);
  }
*/


  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testExternalRefsGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testExternalRefsGeneration" +
                                                    "/assets/TestApi.java"));

    List<File> expectedExceptionFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testExternalRefsGeneration/assets/ModelClassException.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testapi");
    Path pathToException = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testapi/model/exception");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    File targetException = pathToException.toFile();
    assertThat(targetException).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
    validateFiles(expectedExceptionFiles, targetException);
  }



}
