package com.corunet.api.generator.openapi.integrationTest;

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
import com.soebes.itf.jupiter.extension.MavenJupiterExtension;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.maven.MavenProjectResult;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

@MavenRepository
@MavenJupiterExtension
@Execution(ExecutionMode.SAME_THREAD)
public class OpenApiGenerationTest {

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:openapi-generation")
  void testApiClientGeneration(MavenProjectResult result) throws IOException {
    String expectedFileName = "TestApi.java";
    File expectedFile = new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiClientGeneration/assets/TestApi.java");

    List<String> expectedModelFileNames = List.of("ApiErrorDTO.java","ApiTestDTO.java","ApiTestInfoDTO.java");

    List<File> expectedModelFiles = List.of(new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiClientGeneration/assets/ApiErrorDTO.java"),
                                            new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiClientGeneration/assets/ApiTestDTO.java"),
                                            new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiClientGeneration/assets/ApiTestInfoDTO.java")
    );

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetApi = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testApi");
    Path pathToTargetModel = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testApi/model");

    testFile(pathToTargetApi,expectedFileName,expectedFile);

    testGroupFiles(pathToTargetModel, expectedModelFileNames, expectedModelFiles);

  }
  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:openapi-generation")
  void testApiReactiveGeneration(MavenProjectResult result) throws IOException {
    String expectedFileName = "TestApi.java";
    File expectedFile = new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiReactiveGeneration/assets/TestApi.java");

    List<String> expectedModelFileNames = List.of("ApiErrorDTO.java","ApiTestDTO.java","ApiTestInfoDTO.java");

    List<File> expectedModelFiles = List.of(new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiReactiveGeneration/assets/ApiErrorDTO.java"),
                                            new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiReactiveGeneration/assets/ApiTestDTO.java"),
                                            new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiReactiveGeneration/assets/ApiTestInfoDTO.java")
    );

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetApi = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testReactiveApi");
    Path pathToTargetModel = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testReactiveApi/model");

    testFile(pathToTargetApi,expectedFileName,expectedFile);

    testGroupFiles(pathToTargetModel, expectedModelFileNames, expectedModelFiles);

  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:openapi-generation")
  void testApiTagsGeneration(MavenProjectResult result) throws IOException {

    List<String> expectedModelFileNames = List.of("TestTagFirstApi.java","TestTagSecondApi.java");

    List<File> expectedModelFiles = List.of(new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiTagsGeneration/assets/TestTagFirstApi.java"),
                                            new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiTagsGeneration/assets/TestTagSecondApi.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    pathToTarget = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testTags");

    testGroupFiles(pathToTarget, expectedModelFileNames, expectedModelFiles);

  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:openapi-generation")
  void testApiMultiGeneration(MavenProjectResult result) throws IOException {
    String expectedFileNameFirst = "TestFirstApi.java";
    File expectedFileFirst= new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiMultiGeneration/assets/TestFirstApi.java");

    String expectedFileNameSecond = "TestSecondApi.java";
    File expectedFileSecond  = new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testApiMultiGeneration/assets/TestSecondApi.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst  = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testMultiFile/first");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testMultiFile/second");

    testFile(pathToTargetFirst,expectedFileNameFirst,expectedFileFirst);

    testFile(pathToTargetSecond,expectedFileNameSecond,expectedFileSecond);
  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:openapi-generation")
  void testWebClientApiGeneration(MavenProjectResult result) throws IOException {
    String expectedFileNameFirst = "TestApi.java";
    File expectedFileFirst= new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testWebClientApiGeneration/assets/TestApi.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst  = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testWebClient");

    testFile(pathToTargetFirst,expectedFileNameFirst,expectedFileFirst);

  }

  @MavenTest
  @MavenGoal("com.corunet:scs-multiapi-maven-plugin:1.0.0-SNAPSHOT:openapi-generation")
  void testRestClientApiGeneration(MavenProjectResult result) throws IOException {
    String expectedFileNameFirst = "TestApi.java";
    File expectedFileFirst= new File("src/test/resources/com/corunet/api/generator/openapi/integrationTest/OpenApiGenerationTest/testRestClientApiGeneration/assets/TestApi.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst  = pathToTarget.resolve("target/generated-sources/corunet/apigenerator/com/corunet/multifileplugin/testRestClient");

    testFile(pathToTargetFirst,expectedFileNameFirst,expectedFileFirst);

  }


  private void testFile(Path pathToTarget, String expectedFileName, File expectedFile) throws IOException {
    File targetDirectory = pathToTarget.toFile();
    assertThat(targetDirectory).isNotEmptyDirectory();

    assertThat(targetDirectory.list()).contains(expectedFileName);

    List<File> outputFiles = new ArrayList<>(List.of(Objects.requireNonNull(targetDirectory.listFiles())));
    outputFiles.sort(Comparator.comparing(File::getPath));

    InputStream reader1 = new FileInputStream(outputFiles.get(0));
    InputStream reader2 = new FileInputStream(expectedFile);

    assertTrue(contentEqualsIgnoreEOL(new BufferedReader(new InputStreamReader(reader1)), new BufferedReader(new InputStreamReader(reader2))));
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
