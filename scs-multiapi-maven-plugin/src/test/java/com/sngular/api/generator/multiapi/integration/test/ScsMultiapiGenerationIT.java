/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.multiapi.integration.test;

import static com.sngular.api.generator.test.utils.TestUtils.checkTargetFiles;
import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;

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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

@MavenRepository
@MavenJupiterExtension
@Execution(ExecutionMode.SAME_THREAD)
@Disabled("TODO: IFS Target folder problem")
public class ScsMultiapiGenerationIT {

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

    List<String> expectedConsumerFiles = List.of("com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT" +
                                                 "/" + testName + "/assets/IPublishOperation.java",

                                                 "com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName +
                                                 "/assets/TestClassName.java");

    List<String> expectedProducerFiles = List.of(
      "com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName + "/assets/ISubscribeOperation.java",
      "com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName + "/assets/Producer.java");

    List<String> expectedFileNames = List.of("TestApi.java");

    List<String> expectedFiles = List.of(
      "com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName + "/assets/TestApi.java");

    List<String> expectedModelFileNames = List.of("ApiErrorDTO.java", "ApiTestDTO.java", "ApiTestInfoDTO.java");

    List<String> expectedModelFiles = List.of(
      "com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName + "/assets/ApiErrorDTO.java",
      "com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName + "/assets/ApiTestDTO.java",
      "com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName + "/assets/ApiTestInfoDTO.java"    );

    List<String> expectedExceptionFilenames = List.of("ModelClassException.java");

    List<String> expectedExceptionFiles = List.of("com/sngular/api/generator/multiapi/integration/test/ScsMultiapiGenerationIT/" + testName + "/assets" +
                                                  "/ModelClassException.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toAbsolutePath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/com/sngular/generator/multiapi/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/com/sngular/generator/multiapi/model/event/producer");

    Path pathToTargetApi = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/com/sngular/generator/multiapi/rest");
    Path pathToTargetModel = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/com/sngular/generator/multiapi/rest/model");
    Path pathToTargetException = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/com/sngular/generator/multiapi/rest/model/exception");

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
