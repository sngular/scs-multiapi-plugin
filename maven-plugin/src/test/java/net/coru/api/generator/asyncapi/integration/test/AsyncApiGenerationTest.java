/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.asyncapi.integration.test;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
import static net.coru.api.generator.test.utils.TestUtils.checkTargetFiles;
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
public class AsyncApiGenerationTest {

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGeneration(MavenProjectResult result) throws IOException {
    List<String> expectedFileConsumerNames = List.of("TestClassName.java", "IPublishOperation.java");

    List<String> expectedFileProducerNames = List.of("Producer.java", "ISubscribeOperation.java");

    List<File> expectedConsumerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGeneration/assets/IPublishOperation.java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGeneration/assets/TestClassName.java"));

    List<File> expectedProducerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGeneration/assets/ISubscribeOperation.java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGeneration/assets/Producer.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/scsplugin/business_model/model/event/producer");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    File targetProducerDirectory = pathToTargetProducer.toFile();

    checkTargetFiles(expectedFileConsumerNames, targetConsumerDirectory);
    checkTargetFiles(expectedFileProducerNames, targetProducerDirectory);
    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationTwoYmlFiles(MavenProjectResult result) throws IOException {
    List<String> expectedFirstYmlFileNames = List.of("IPublishOperation.java", "ISubscribeOperation.java", "Producer.java", "Subscriber.java");

    List<String> expectedSecondYmlFileNames = List.of("Producer.java", "IPublishOperation2.java");

    List<File> expectedFirstYmlFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/IPublishOperation.java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/ISubscribeOperation.java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/Producer.java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/Subscriber.java"));

    List<File> expectedSecondYmlFiles = List.of(
        new File(
            "src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/producer2/IPublishOperation2.java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/producer2/Producer.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetFirstYml = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru");
    Path pathToTargetSecondYml = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/scsplugin/business_model/model/event/producer2");

    File targetFirstYmlDirectory = pathToTargetFirstYml.toFile();
    File targetSecondYmlDirectory = pathToTargetSecondYml.toFile();

    checkTargetFiles(expectedFirstYmlFileNames, targetFirstYmlDirectory);
    checkTargetFiles(expectedSecondYmlFileNames, targetSecondYmlDirectory);
    validateFiles(expectedFirstYmlFiles, targetFirstYmlDirectory);
    validateFiles(expectedSecondYmlFiles, targetSecondYmlDirectory);

  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationExternalAvro(MavenProjectResult result) throws IOException {
    List<String> expectedFileProducerNames = List.of("IPublishOperation.java", "Producer.java");

    List<String> expectedFileConsumerNames = List.of("ISubscribeOperation.java", "Subscriber.java");

    List<File> expectedProducerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/IPublishOperation" +
                 ".java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/Producer" +
                 ".java"));

    List<File> expectedConsumerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/ISubscribeOperation" +
                 ".java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/Subscriber" +
                 ".java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/scsplugin/business_model/model/event/producer");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    File targetProducerDirectory = pathToTargetProducer.toFile();

    checkTargetFiles(expectedFileConsumerNames, targetConsumerDirectory);
    checkTargetFiles(expectedFileProducerNames, targetProducerDirectory);
    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationStreamBridge(MavenProjectResult result) throws IOException {
    List<File> expectedConsumerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationStreamBridge/assets/ISubscribeOperation.java"),
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationStreamBridge/assets/TestClassName.java"));

    List<File> expectedProducerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationStreamBridge/assets/StreamBridgeProducer" +
                 ".java"));

    commonTest(result, expectedConsumerFiles, expectedProducerFiles);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationWithoutOperationIds(MavenProjectResult result) throws IOException {
    List<File> expectedConsumerFiles = List.of(new File(
                                                   "src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest" +
                                                   "/testFileGenerationWithoutOperationIds/assets/ISubscribeOperation.java"),
                                               new File(
                                                   "src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest" +
                                                   "/testFileGenerationWithoutOperationIds/assets/TestClassName.java"));

    List<File> expectedProducerFiles = List.of(
        new File("src/test/resources/net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationWithoutOperationIds/assets" +
                 "/StreamBridgeProducer.java"));
    commonTest(result, expectedConsumerFiles, expectedProducerFiles);
  }

  private void commonTest(final MavenProjectResult result, final List<File> expectedConsumerFiles, final List<File> expectedProducerFiles) throws IOException {
    List<String> expectedFileConsumerNames = List.of("TestClassName.java", "ISubscribeOperation.java");

    List<String> expectedFileProducerNames = List.of("StreamBridgeProducer.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/scsplugin/business_model/model/event/producer");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    File targetProducerDirectory = pathToTargetProducer.toFile();

    checkTargetFiles(expectedFileConsumerNames, targetConsumerDirectory);
    checkTargetFiles(expectedFileProducerNames, targetProducerDirectory);
    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }
}
