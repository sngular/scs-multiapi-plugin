package com.corunet.api.generator.asyncapi.integrationTest;

import static com.corunet.api.generator.testUtils.TestUtils.validateFiles;
import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;

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
    List<String> expectedFileConsumerNames = List.of("NombreDeClase.java", "IPublishOperation.java");

    List<String> expectedFileProducerNames = List.of("Producer.java", "ISubscribeOperation.java");

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGeneration/assets/IPublishOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGeneration/assets/NombreDeClase.java"));

    List<File> expectedProducerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGeneration/assets/ISubscribeOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGeneration/assets/Producer.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/producer");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    assertThat(targetConsumerDirectory).isNotEmptyDirectory();

    File targetProducerDirectory = pathToTargetProducer.toFile();
    assertThat(targetProducerDirectory).isNotEmptyDirectory();

    assertThat(targetConsumerDirectory.list()).containsAll(expectedFileConsumerNames);
    assertThat(targetProducerDirectory.list()).containsAll(expectedFileProducerNames);

    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationTwoYmlFiles(MavenProjectResult result) throws IOException {
    List<String> expectedFirstYmlFileNames = List.of("IPublishOperation.java", "ISubscribeOperation.java", "Producer.java", "Subscriber.java");

    List<String> expectedSecondYmlFileNames = List.of("Producer.java", "IPublishOperation2.java");

    List<File> expectedFirstYmlFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/IPublishOperation.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/ISubscribeOperation.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/Producer.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/Subscriber.java"));

    List<File> expectedSecondYmlFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/producer2/IPublishOperation2.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/producer2/Producer.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetFirstYml = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet");
    Path pathToTargetSecondYml = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/scsplugin/business_model/model/event/producer2");

    File targetFirstYmlDirectory = pathToTargetFirstYml.toFile();
    assertThat(targetFirstYmlDirectory).isNotEmptyDirectory();

    File targetSecondYmlDirectory = pathToTargetSecondYml.toFile();
    assertThat(targetSecondYmlDirectory).isNotEmptyDirectory();

    assertThat(targetFirstYmlDirectory.list()).containsAll(expectedFirstYmlFileNames);
    assertThat(targetSecondYmlDirectory.list()).containsAll(expectedSecondYmlFileNames);

    validateFiles(expectedFirstYmlFiles, targetFirstYmlDirectory);
    validateFiles(expectedSecondYmlFiles, targetSecondYmlDirectory);

  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationExternalAvro(MavenProjectResult result) throws IOException {
    List<String> expectedFileProducerNames = List.of("IPublishOperation.java", "Producer.java");

    List<String> expectedFileConsumerNames = List.of("ISubscribeOperation.java", "Subscriber.java");

    List<File> expectedProducerFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/IPublishOperation" +
                 ".java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/Producer" +
                 ".java"));

    List<File> expectedConsumerFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/ISubscribeOperation" +
                 ".java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationExternalAvro/assets/Subscriber" +
                 ".java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/producer");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    assertThat(targetConsumerDirectory).isNotEmptyDirectory();

    File targetProducerDirectory = pathToTargetProducer.toFile();
    assertThat(targetProducerDirectory).isNotEmptyDirectory();

    assertThat(targetConsumerDirectory.list()).containsAll(expectedFileConsumerNames);
    assertThat(targetProducerDirectory.list()).containsAll(expectedFileProducerNames);

    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationStreamBridge(MavenProjectResult result) throws IOException {
    List<String> expectedFileConsumerNames = List.of("NombreDeClase.java", "IPublishOperation.java");

    List<String> expectedFileProducerNames = List.of("StreamBridgeProducer.java");

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationStreamBridge/assets/IPublishOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationStreamBridge/assets/NombreDeClase.java"));

    List<File> expectedProducerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationStreamBridge/assets/StreamBridgeProducer" +
                                                        ".java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/producer");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    assertThat(targetConsumerDirectory).isNotEmptyDirectory();

    File targetProducerDirectory = pathToTargetProducer.toFile();
    assertThat(targetProducerDirectory).isNotEmptyDirectory();

    assertThat(targetConsumerDirectory.list()).containsAll(expectedFileConsumerNames);
    assertThat(targetProducerDirectory.list()).containsAll(expectedFileProducerNames);

    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationWithoutOperationIds(MavenProjectResult result) throws IOException {
    List<String> expectedFileConsumerNames = List.of("NombreDeClase.java", "ISubscribeOperation.java");

    List<String> expectedFileProducerNames = List.of("StreamBridgeProducer.java");

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationWithoutOperationIds/assets/ISubscribeOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationWithoutOperationIds/assets/NombreDeClase.java"));

    List<File> expectedProducerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/AsyncApiGenerationTest/testFileGenerationWithoutOperationIds/assets" +
                                                        "/StreamBridgeProducer.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/producer");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    assertThat(targetConsumerDirectory).isNotEmptyDirectory();

    File targetProducerDirectory = pathToTargetProducer.toFile();
    assertThat(targetProducerDirectory).isNotEmptyDirectory();

    assertThat(targetConsumerDirectory.list()).containsAll(expectedFileConsumerNames);
    assertThat(targetProducerDirectory.list()).containsAll(expectedFileProducerNames);

    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
  }
}
