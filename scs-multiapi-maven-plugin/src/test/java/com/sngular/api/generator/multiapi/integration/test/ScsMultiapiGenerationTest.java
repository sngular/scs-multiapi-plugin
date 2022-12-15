/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.multiapi.integration.test;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
import static com.sngular.api.generator.test.utils.TestUtils.checkTargetFiles;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.sngular.api.generator.test.utils.TestUtils;
import com.soebes.itf.jupiter.extension.MavenJupiterExtension;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.extension.SystemProperty;
import com.soebes.itf.jupiter.maven.MavenProjectResult;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

@MavenRepository
@MavenJupiterExtension
@Execution(ExecutionMode.SAME_THREAD)
public class ScsMultiapiGenerationTest {

  @MavenTest
  @SystemProperty(value = "groupId", content = "${project.groupId}")
  @SystemProperty(value = "artifactId", content = "${project.artifactId}")
  @SystemProperty(value = "version", content = "${project.version}")
  void testScsMultiapiGeneration(MavenProjectResult result) throws IOException {

    commonTest("testScsMultiapiGeneration", "generated-sources", result);

  }

  @MavenTest
  @SystemProperty(value = "groupId", content = "${project.groupId}")
  @SystemProperty(value = "artifactId", content = "${project.artifactId}")
  @SystemProperty(value = "version", content = "${project.version}")
  void testMultiapiGeneratedSourcesFolder(MavenProjectResult result) throws IOException {

    commonTest("testMultiapiGeneratedSourcesFolder", "sources-generated", result);

  }

  private void commonTest(String testName, String generatedSourcesFolderName, MavenProjectResult result) throws IOException {
    List<String> expectedFileConsumerNames = List.of("TestClassName.java", "IPublishOperation.java");

    List<String> expectedFileProducerNames = List.of("Producer.java", "ISubscribeOperation.java");

    List<String> expectedConsumerFiles = List.of("net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest" +
                                                        "/" + testName + "/assets/IPublishOperation.java",
                                               
                                                   "net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "" +
                                                   "/assets/TestClassName.java");

    List<String> expectedProducerFiles = List.of(
        "net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ISubscribeOperation.java",
        "net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/Producer.java");

    List<String> expectedFileNames = List.of("TestApi.java");

    List<String> expectedFiles = List.of(
        "net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/TestApi.java");

    List<String> expectedModelFileNames = List.of("ApiErrorDTO.java", "ApiTestDTO.java", "ApiTestInfoDTO.java");

    List<String> expectedModelFiles = List.of(
        "net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ApiErrorDTO.java",
        "net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ApiTestDTO.java",
        "net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ApiTestInfoDTO.java"    );

    List<String> expectedExceptionFilenames = List.of("ModelClassException.java");

    List<String> expectedExceptionFiles = List.of("net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets" +
                                                         "/ModelClassException.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/generator/multiapi/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/generator/multiapi/model/event/producer");

    Path pathToTargetApi = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/generator/multiapi/rest");
    Path pathToTargetModel = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/generator/multiapi/rest/model");
    Path pathToTargetException = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/generator/multiapi/rest/model/exception");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    File targetProducerDirectory = pathToTargetProducer.toFile();
    File targetApiDirectory = pathToTargetApi.toFile();
    File targetModelDirectory = pathToTargetModel.toFile();
    File targetExceptionDirectory = pathToTargetException.toFile();

    checkTargetFiles(expectedFileConsumerNames, targetConsumerDirectory);
    checkTargetFiles(expectedFileProducerNames, targetProducerDirectory);
    checkTargetFiles(expectedFileNames, targetApiDirectory);
    checkTargetFiles(expectedModelFileNames, targetModelDirectory);
    checkTargetFiles(expectedExceptionFilenames, targetExceptionDirectory);

    TestUtils.validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    TestUtils.validateFiles(expectedProducerFiles, targetProducerDirectory);
    TestUtils.validateFiles(expectedFiles, targetApiDirectory);
    TestUtils.validateFiles(expectedModelFiles, targetModelDirectory);
    TestUtils.validateFiles(expectedExceptionFiles, targetExceptionDirectory);
  }
}
