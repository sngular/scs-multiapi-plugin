package com.corunet.api.generator.asyncapi.integrationTest;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
import static junit.framework.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

import com.soebes.itf.jupiter.extension.MavenGoal;
import com.soebes.itf.jupiter.extension.MavenJupiterExtension;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.maven.MavenProjectResult;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

@MavenRepository
@MavenJupiterExtension
@Execution(ExecutionMode.SAME_THREAD)
public class BaseTest {

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:asyncapi-generation")
  void testFileGeneration(MavenProjectResult result) throws IOException {
    List<String> expectedFileConsumerNames = List.of("NombreDeClase.java", "IPublishOperation.java");

    List<String> expectedFileProducerNames = List.of("Producer.java", "ISubscribeOperation.java");

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGeneration/assets/IPublishOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGeneration/assets/NombreDeClase.java"));

    List<File> expectedProducerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGeneration/assets/ISubscribeOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGeneration/assets/Producer.java"));

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

    validatedFiles(expectedConsumerFiles, expectedProducerFiles, targetConsumerDirectory, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:asyncapi-generation")
  void testFileGenerationTwoYmlFiles(MavenProjectResult result) throws IOException {
    List<String> expectedFirstYmlFileNames = List.of("IPublishOperation.java", "ISubscribeOperation.java", "Producer.java", "Subscriber.java");

    List<String> expectedSecondYmlFileNames = List.of("Producer.java", "IPublishOperation2.java");

    List<File> expectedFirstYmlFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationTwoYmlFiles/assets/IPublishOperation.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationTwoYmlFiles/assets/ISubscribeOperation.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationTwoYmlFiles/assets/Producer.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationTwoYmlFiles/assets/Subscriber.java"));

    List<File> expectedSecondYmlFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationTwoYmlFiles/assets/producer2/IPublishOperation2.java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationTwoYmlFiles/assets/producer2/Producer.java"));

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

    validatedFiles(expectedFirstYmlFiles, expectedSecondYmlFiles, targetFirstYmlDirectory, targetSecondYmlDirectory);

  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:asyncapi-generation")
  void testFileGenerationExternalAvro(MavenProjectResult result) throws IOException {
    List<String> expectedFileProducerNames = List.of("IPublishOperation.java", "Producer.java");

    List<String> expectedFileConsumerNames = List.of("ISubscribeOperation.java", "Subscriber.java");

    List<File> expectedProducerFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationExternalAvro/assets/IPublishOperation" +
                 ".java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationExternalAvro/assets/Producer" +
                 ".java"));

    List<File> expectedConsumerFiles = List.of(
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationExternalAvro/assets/ISubscribeOperation" +
                 ".java"),
        new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationExternalAvro/assets/Subscriber" +
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

    validatedFiles(expectedConsumerFiles, expectedProducerFiles, targetConsumerDirectory, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:asyncapi-generation")
  void testFileGenerationStreamBridge(MavenProjectResult result) throws IOException {
    List<String> expectedFileConsumerNames = List.of("NombreDeClase.java", "IPublishOperation.java");

    List<String> expectedFileProducerNames = List.of("StreamBridgeProducer.java");

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationStreamBridge/assets/IPublishOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationStreamBridge/assets/NombreDeClase.java"));

    List<File> expectedProducerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationStreamBridge/assets/StreamBridgeProducer" +
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

    validatedFiles(expectedConsumerFiles, expectedProducerFiles, targetConsumerDirectory, targetProducerDirectory);
  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:asyncapi-generation")
  void testFileGenerationWithoutOperationIds(MavenProjectResult result) throws IOException {
    List<String> expectedFileConsumerNames = List.of("NombreDeClase.java", "ISubscribeOperation.java");

    List<String> expectedFileProducerNames = List.of("StreamBridgeProducer.java");

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationWithoutOperationIds/assets/ISubscribeOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationWithoutOperationIds/assets/NombreDeClase.java"));

    List<File> expectedProducerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/asyncapi/integrationTest/BaseTest/testFileGenerationWithoutOperationIds/assets" +
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

    validatedFiles(expectedConsumerFiles, expectedProducerFiles, targetConsumerDirectory, targetProducerDirectory);
  }

  private void validatedFiles(
      final List<File> expectedConsumerFiles, final List<File> expectedProducerFiles, final File targetConsumerDirectory, final File targetProducerDirectory)
      throws IOException {
    FileInputStream reader1;
    FileInputStream reader2;

    List<File> outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetConsumerDirectory.listFiles())));
    outputFiles.sort(Comparator.comparing(File::getPath));

    for (int i = 0; i < outputFiles.size(); i++) {
      reader1 = new FileInputStream(outputFiles.get(i));
      reader2 = new FileInputStream(expectedConsumerFiles.get(i));
      assertTrue(IOUtils.contentEquals(reader1, reader2));
    }

    outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetProducerDirectory.listFiles())));
    outputFiles.sort(Comparator.comparing(File::getPath));

    for (int i = 0; i < outputFiles.size(); i++) {
      reader1 = new FileInputStream(outputFiles.get(i));
      reader2 = new FileInputStream(expectedProducerFiles.get(i));
      assertTrue(IOUtils.contentEquals(reader1, reader2));
    }
  }
}
