/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.multiapi.integration.test;

import static net.coru.api.generator.test.utils.TestUtils.checkTargetFiles;
import static net.coru.api.generator.test.utils.TestUtils.validateFiles;
import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

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

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest" +
                                                        "/" + testName + "/assets/IPublishOperation.java"),
                                               new File(
                                                   "src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "" +
                                                   "/assets/TestClassName.java"));

    List<File> expectedProducerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ISubscribeOperation.java"),
        new File("src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/Producer.java"));

    List<String> expectedFileNames = List.of("TestApi.java");

    List<File> expectedFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets" +
                 "/TestApi.java"));

    List<String> expectedModelFileNames = List.of("ApiErrorDTO.java", "ApiTestDTO.java", "ApiTestInfoDTO.java");

    List<File> expectedModelFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ApiErrorDTO.java"),
        new File("src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ApiTestDTO.java"),
        new File("src/test/resources/net/coru/api/generator/multiapi/integration/test/ScsMultiapiGenerationTest/" + testName + "/assets/ApiTestInfoDTO.java")
    );

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/scsplugin/business_model/model/event/producer");

    Path pathToTargetApi = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/multifileplugin/testapi");
    Path pathToTargetModel = pathToTarget.resolve("target/" + generatedSourcesFolderName + "/apigenerator/net/coru/multifileplugin/testapi/model");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    File targetProducerDirectory = pathToTargetProducer.toFile();
    File targetApiDirectory = pathToTargetApi.toFile();
    File targetModelDirectory = pathToTargetModel.toFile();

    checkTargetFiles(expectedFileConsumerNames, targetConsumerDirectory);
    checkTargetFiles(expectedFileProducerNames, targetProducerDirectory);
    checkTargetFiles(expectedFileNames, targetApiDirectory);
    checkTargetFiles(expectedModelFileNames, targetModelDirectory);

    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
    validateFiles(expectedFiles, targetApiDirectory);
    validateFiles(expectedModelFiles, targetModelDirectory);
  }
}
