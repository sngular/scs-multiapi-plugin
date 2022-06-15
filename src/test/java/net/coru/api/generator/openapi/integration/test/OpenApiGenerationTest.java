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

    commonTest(result, expectedFile, expectedModelFiles);
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

    commonTest(result, expectedFile, expectedModelFiles);
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

    commonTest(result, expectedFile, expectedModelFiles);
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

    commonTest(result, expectedFile, expectedModelFiles);
  }

  private void commonTest(final MavenProjectResult result, final List<File> expectedFile, final List<File> expectedModelFiles) throws IOException {
    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetApi = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testapi");
    Path pathToTargetModel = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testapi/model");

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
    pathToTarget = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testtags");

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
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testmultifile/first");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testmultifile/second");

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
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testwebclient");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testClientPackageWebClientApiGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testClientPackageWebClientApiGeneration/assets" +
                                                    "/TestClient.java"));
    List<File> expectedFileSecond = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testClientPackageWebClientApiGeneration/assets" +
                                                     "/TestAuth.java"),
                                            new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testClientPackageWebClientApiGeneration/assets" +
                                                     "/TestHttpBasicAuth.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testclientpackage/client");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testclientpackage/client/auth");

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
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/net/coru/multifileplugin/testrestclient");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
  }
}
