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

  private final String DEFAULT_TARGET_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi";

  private final String DEFAULT_MODEL_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi/model";

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiClientGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiClientGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiClientGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiClientGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiClientGeneration/assets/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiEnumsGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsGeneration/assets/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiEnumsLombokGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiEnumsLombokGeneration/assets/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiPathParameterGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathParameterGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathParameterGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathParameterGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathParameterGeneration/assets/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testMultipleRefGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testMultipleRefGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testMultipleRefGeneration/assets/InlineResponse200CreateGameDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testMultipleRefGeneration/assets/MessageDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiReactiveGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiReactiveGeneration/assets" +
                                               "/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiReactiveGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiReactiveGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiReactiveGeneration/assets/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  private void commonTest(final MavenProjectResult result, final List<File> expectedFile, final List<File> expectedModelFiles, final String targetApi, final String targetModel)
      throws IOException {
    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetApi = pathToTarget.resolve(targetApi);
    Path pathToTargetModel = pathToTarget.resolve(targetModel);

    File targetApiFolder = pathToTargetApi.toFile();
    assertThat(targetApiFolder).isNotEmptyDirectory();

    File targetModelFolder = pathToTargetModel.toFile();
    assertThat(targetModelFolder).isNotEmptyDirectory();

    validateFiles(expectedFile, targetApiFolder);
    validateFiles(expectedModelFiles, targetModelFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiTagsGeneration(MavenProjectResult result) throws IOException {

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiTagsGeneration/assets/TestTagFirstApi.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiTagsGeneration/assets/TestTagSecondApi.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    pathToTarget = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testtags");

    File targetApiFolder = pathToTarget.toFile();
    assertThat(targetApiFolder).isNotEmptyDirectory();

    validateFiles(expectedModelFiles, targetApiFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiMultiGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                    "/TestFirstApi.java"));
    List<File> expectedFileSecond = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                     "/TestSecondApi.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/first");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/second");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    File targetSecondFolder = pathToTargetSecond.toFile();
    assertThat(targetSecondFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
    validateFiles(expectedFileSecond, targetSecondFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testWebClientApiGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testWebClientApiGeneration/assets" +
                 "/TestApi.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testwebclient");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testClientPackageWebClientApiGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testClientPackageWebClientApiGeneration/assets" +
                 "/TestClient.java"));
    List<File> expectedFileSecond = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testClientPackageWebClientApiGeneration/assets" +
                 "/TestAuth.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testClientPackageWebClientApiGeneration/assets" +
                 "/TestHttpBasicAuth.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testclientpackage/client");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testclientpackage/client/auth");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    File targetSecondFolder = pathToTargetSecond.toFile();
    assertThat(targetSecondFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
    validateFiles(expectedFileSecond, targetSecondFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testRestClientApiGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testRestClientApiGeneration" +
                                                    "/assets/TestApi.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testrestclient");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testExternalRefsGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testExternalRefsGeneration" +
                                                    "/assets/TestApi.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testapi");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiPathWithBarsGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathWithBarsGeneration/assets/TestApi.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathWithBarsGeneration/assets/TestSchemaApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathWithBarsGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathWithBarsGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiPathWithBarsGeneration/assets/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiParametersWithContentGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiParametersWithContentGeneration/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiParametersWithContentGeneration/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiParametersWithContentGeneration/assets" +
                 "/ApiInlineParameterShowTestByIdTestIdDTO" +
                 ".java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiParametersWithContentGeneration/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiParametersWithContentGeneration/assets/ApiTestInfoDTO.java")

    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testOverWriteModelTrue(MavenProjectResult result) throws IOException {
    List<File> expectedFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testOverWriteModelTrue/assets/TestApi.java"));

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testOverWriteModelTrue/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testOverWriteModelTrue/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testOverWriteModelTrue/assets/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedFile, expectedModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testAllOf(MavenProjectResult result) throws IOException {

    List<File> expectedTestApiFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/testApi/TestApi.java"));

    List<File> expectedTestApiModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/testApi/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/testApi/ApiTestAllOfDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/testApi/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/testApi/ApiTestInfoDTO.java")

    );

    List<File> expectedLombokFile = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/lombok/TestApi.java"));

    List<File> expectedLombokModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/lombok/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/lombok/ApiTestAllOfDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/lombok/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testAllOf/assets/lombok/ApiTestInfoDTO.java")
    );

    commonTest(result, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API);
    commonTest(result, expectedLombokFile, expectedLombokModelFiles, "target/generated-sources/apigenerator/net/coru/multifileplugin/lombok/testapi",
               "target/generated-sources/apigenerator/net/coru/multifileplugin/lombok/testapi/model");
  }

}
