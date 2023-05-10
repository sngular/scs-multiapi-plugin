/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.asyncapi.integration.test;

import static com.sngular.api.generator.test.utils.TestUtils.checkTargetFiles;
import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.sngular.api.generator.test.utils.TestUtils;
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
public class AsyncApiGenerationIT {

  @MavenTest
  @MavenGoal("generate-sources")
  void testFileGenerationTwoYmlFiles(MavenProjectResult result) throws IOException {
    List<String> expectedFirstYmlFileNames = List.of("IPublishOperation.java", "ISubscribeOperation.java", "Producer.java", "Subscriber.java");

    List<String> expectedSecondYmlFileNames = List.of("Producer.java", "IPublishOperation2.java");

    List<String> expectedFirstYmlFiles = List.of(
      "com/sngular/api/generator/asyncapi/integration/test/AsyncApiGenerationIT/testFileGenerationTwoYmlFiles/assets/IPublishOperation.java",
      "com/sngular/api/generator/asyncapi/integration/test/AsyncApiGenerationIT/testFileGenerationTwoYmlFiles/assets/ISubscribeOperation.java",
      "com/sngular/api/generator/asyncapi/integration/test/AsyncApiGenerationIT/testFileGenerationTwoYmlFiles/assets/Producer.java",
      "com/sngular/api/generator/asyncapi/integration/test/AsyncApiGenerationIT/testFileGenerationTwoYmlFiles/assets/Subscriber.java");

    List<String> expectedSecondYmlFiles = List.of(
      "com/sngular/api/generator/asyncapi/integration/test/AsyncApiGenerationIT/testFileGenerationTwoYmlFiles/assets/producer2/IPublishOperation2.java",
      "com/sngular/api/generator/asyncapi/integration/test/AsyncApiGenerationIT/testFileGenerationTwoYmlFiles/assets/producer2/Producer.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toAbsolutePath();

    Path pathToTargetFirstYml = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/apigenerator/asyncapi");
    Path pathToTargetSecondYml = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/apigenerator/asyncapi/producer2");

    File targetFirstYmlDirectory = pathToTargetFirstYml.toFile();
    File targetSecondYmlDirectory = pathToTargetSecondYml.toFile();

    checkTargetFiles(expectedFirstYmlFileNames, targetFirstYmlDirectory);
    checkTargetFiles(expectedSecondYmlFileNames, targetSecondYmlDirectory);
    TestUtils.validateFiles(expectedFirstYmlFiles, targetFirstYmlDirectory);
    TestUtils.validateFiles(expectedSecondYmlFiles, targetSecondYmlDirectory);

  }

  @MavenTest
  @MavenGoal("generate-sources")
  void testDependencyYml(MavenProjectResult result) throws IOException {
    List<String> expectedProducerFiles = List.of("Producer.java", "IPublishOperationFileGeneration.java");
    List<String> expectedMessageFiles = List.of("OrderCreated.java");
    List<String> expectedSchemaFiles = List.of("Order.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toAbsolutePath();
    File targetProducerDirectory = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/apigenerator/asyncapi").toFile();
    File targetMessageDirectory = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/apigenerator/asyncapi/model/messages").toFile();
    File targetSchemaDirectory = pathToTarget.resolve("target/generated-sources/apigenerator/com/sngular/apigenerator/asyncapi/model/schemas").toFile();

    checkTargetFiles(expectedProducerFiles, targetProducerDirectory);
    checkTargetFiles(expectedMessageFiles, targetMessageDirectory);
    checkTargetFiles(expectedSchemaFiles, targetSchemaDirectory);
  }
}
