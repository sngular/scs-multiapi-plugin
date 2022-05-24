package com.corunet.api.generator.multiapi.integrationTest;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
import static junit.framework.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

import com.soebes.itf.jupiter.extension.MavenGoal;
import com.soebes.itf.jupiter.extension.MavenGoals;
import com.soebes.itf.jupiter.extension.MavenJupiterExtension;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.extension.SystemProperties;
import com.soebes.itf.jupiter.extension.SystemProperty;
import com.soebes.itf.jupiter.maven.MavenProjectResult;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
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

    validatedFiles(expectedConsumerFiles, expectedProducerFiles, targetConsumerDirectory, targetProducerDirectory);
    testGroupFiles(pathToTargetApi, expectedFileNames, expectedFiles);
    testGroupFiles(pathToTargetModel, expectedModelFileNames, expectedModelFiles);
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


  private void testGroupFiles(Path pathToTargetModel,List<String> expectedFileNames, List<File> expectedFiles) throws IOException {
    File targetModelDirectory = pathToTargetModel.toFile();
    assertThat(targetModelDirectory).isNotEmptyDirectory();
    assertThat(targetModelDirectory.list()).containsAll(expectedFileNames);

    List<File> outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetModelDirectory.listFiles())));
    outputFiles.sort(Comparator.comparing(File::getPath));

    InputStream reader1;
    InputStream reader2;

    for (int i = 0; i < outputFiles.size(); i++) {
      if(!outputFiles.get(i).getPath().endsWith("model")){
        reader1 = new FileInputStream(outputFiles.get(i));
        reader2 = new FileInputStream(expectedFiles.get(i));
        assertTrue(contentEqualsIgnoreEOL(new BufferedReader(new InputStreamReader(reader1)), new BufferedReader(new InputStreamReader(reader2))));
      }
    }
  }


  private boolean contentEqualsIgnoreEOL(final Reader reader1, final Reader reader2)
      throws IOException {
    if (reader1 == reader2) {
      return true;
    }
    if (reader1 == null ^ reader2 == null) {
      return false;
    }
    final BufferedReader br1 = toBufferedReader(reader1);
    final BufferedReader br2 = toBufferedReader(reader2);

    String line1 = customLine(br1.readLine());
    String line2 = customLine(br2.readLine());
    while (line1 != null && line1.equals(line2)) {
      line1 = customLine(br1.readLine());
      line2 = customLine(br2.readLine());
    }
    return Objects.equals(line1, line2);
  }

  private static BufferedReader toBufferedReader(final Reader reader) {
    return reader instanceof BufferedReader ? (BufferedReader) reader : new BufferedReader(reader);
  }

  private static String customLine(String line){
    var lineReturn = "";
    if(StringUtils.isNotBlank(line)){
      lineReturn = line.replace(" ","");
    } else if (null == line) {
      return null;
    }
    return lineReturn;
  }
}
