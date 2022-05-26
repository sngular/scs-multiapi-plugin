package com.corunet.api.generator.multiapi.integrationTest;

import static com.corunet.api.generator.testUtils.TestUtils.validateFiles;
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
  @SystemProperty(value = "groupId",content = "com.corunet")
  @SystemProperty(value = "artifactId",content = "scs-multiapi-maven-plugin")
  @SystemProperty(value = "version",content = "1.0.0-SNAPSHOT")
  void testScsMultiapiGeneration(MavenProjectResult result) throws IOException {

    List<String> expectedFileConsumerNames = List.of("NombreDeClase.java", "IPublishOperation.java");

    List<String> expectedFileProducerNames = List.of("Producer.java", "ISubscribeOperation.java");

    List<File> expectedConsumerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest" +
                                                        "/testScsMultiapiGeneration/assets/IPublishOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest/testScsMultiapiGeneration/assets/NombreDeClase.java"));

    List<File> expectedProducerFiles = List.of(new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest/testScsMultiapiGeneration/assets/ISubscribeOperation.java"),
                                               new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest/testScsMultiapiGeneration/assets/Producer.java"));

    List<String> expectedFileNames = List.of("TestApi.java");

    List<File>  expectedFiles = List.of(new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest/testScsMultiapiGeneration/assets" +
                                                "/TestApi.java"));

    List<String> expectedModelFileNames = List.of("ApiErrorDTO.java","ApiTestDTO.java","ApiTestInfoDTO.java");

    List<File> expectedModelFiles = List.of(new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest/testScsMultiapiGeneration/assets/ApiErrorDTO.java"),
                                            new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest/testScsMultiapiGeneration/assets/ApiTestDTO.java"),
                                            new File("src/test/resources/com/corunet/api/generator/multiapi/integrationTest/ScsMultiapiGenerationTest/testScsMultiapiGeneration/assets/ApiTestInfoDTO.java")
    );


    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetConsumer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/consumer");
    Path pathToTargetProducer = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/scsplugin/business_model/model/event/producer");

    Path pathToTargetApi = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testApi");
    Path pathToTargetModel = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testApi/model");

    File targetConsumerDirectory = pathToTargetConsumer.toFile();
    assertThat(targetConsumerDirectory).isNotEmptyDirectory();

    File targetProducerDirectory = pathToTargetProducer.toFile();
    assertThat(targetProducerDirectory).isNotEmptyDirectory();

    File targetApiDirectory = pathToTargetApi.toFile();
    assertThat(targetApiDirectory).isNotEmptyDirectory();

    File targetModelDirectory = pathToTargetModel.toFile();
    assertThat(targetModelDirectory).isNotEmptyDirectory();

    assertThat(targetConsumerDirectory.list()).containsAll(expectedFileConsumerNames);
    assertThat(targetProducerDirectory.list()).containsAll(expectedFileProducerNames);
    assertThat(targetApiDirectory.list()).containsAll(expectedFileNames);
    assertThat(targetModelDirectory.list()).containsAll(expectedModelFileNames);

    validateFiles(expectedConsumerFiles, targetConsumerDirectory);
    validateFiles(expectedProducerFiles, targetProducerDirectory);
    validateFiles(expectedFiles, targetApiDirectory);
    validateFiles(expectedModelFiles, targetModelDirectory);
  }
}
