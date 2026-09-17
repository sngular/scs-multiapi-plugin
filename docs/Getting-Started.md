# Getting Started with SCS MultiAPI Plugin

## Prerequisites

- **Java**: 11+ (Java 21 recommended)
- **Maven**: 3.9.6+ OR **Gradle**: 7.0+
- **Spring Boot**: 2.x, 3.x, or 4.x
- **IDE**: Any Java IDE (IntelliJ, Eclipse, VS Code, etc.)

## Installation

### Maven Installation

1. **Add plugin to your `pom.xml`**:

```xml
<plugins>
  <plugin>
    <groupId>com.sngular</groupId>
    <artifactId>scs-multiapi-maven-plugin</artifactId>
    <version>7.1.3</version>
    <executions>
      <execution>
        <phase>generate-sources</phase>
        <goals>
          <goal>openapi-generation</goal>
          <!-- OR use: <goal>asyncapi-generation</goal> -->
        </goals>
        <configuration>
          <specFiles>
            <specFile>
              <filePath>${project.basedir}/src/main/resources/api.yml</filePath>
            </specFile>
          </specFiles>
        </configuration>
      </execution>
    </executions>
  </plugin>
</plugins>
```

2. **Add required dependencies**:

```xml
<dependencies>
  <dependency>
    <groupId>io.swagger.parser.v3</groupId>
    <artifactId>swagger-parser-core</artifactId>
    <version>2.1.20</version>
  </dependency>
  <dependency>
    <groupId>io.swagger.core.v3</groupId>
    <artifactId>swagger-annotations-jakarta</artifactId>
    <version>2.2.20</version>
  </dependency>
  <dependency>
    <groupId>jakarta.validation</groupId>
    <artifactId>jakarta.validation-api</artifactId>
    <version>3.0.2</version>
  </dependency>
</dependencies>
```

### Gradle Installation

1. **Add plugin to your `build.gradle`**:

```groovy
plugins {
  id 'com.sngular.scs-multiapi-gradle-plugin' version '7.1.3'
}
```

2. **Add required dependencies**:

```groovy
dependencies {
  implementation 'io.swagger.parser.v3:swagger-parser-core:2.1.20'
  implementation 'io.swagger.core.v3:swagger-annotations-jakarta:2.2.20'
  implementation 'jakarta.validation:jakarta.validation-api:3.0.2'
}
```

3. **Configure plugin** (in `build.gradle`):

```groovy
openapimodel {  // or asyncapimodel for AsyncAPI
  specFile {
    filePath = 'src/main/resources/api.yml'
  }
  overWriteModel = true
}
```

## Your First Generated API

### Step 1: Create API Specification

Create `src/main/resources/api.yml`:

```yaml
openapi: 3.0.0
info:
  title: Pet Store API
  version: 1.0.0
description: A simple Pet Store API
paths:
  /pets:
    get:
      operationId: getPets
      summary: Get all pets
      responses:
        '200':
          description: List of pets
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/Pet'
    post:
      operationId: createPet
      summary: Create a new pet
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/PetInput'
      responses:
        '201':
          description: Pet created
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Pet'
  /pets/{petId}:
    get:
      operationId: getPetById
      summary: Get pet by ID
      parameters:
        - name: petId
          in: path
          required: true
          schema:
            type: integer
      responses:
        '200':
          description: Pet found
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Pet'
        '404':
          description: Pet not found

components:
  schemas:
    Pet:
      type: object
      properties:
        id:
          type: integer
          description: Pet ID
        name:
          type: string
          description: Pet name
        type:
          type: string
          enum: [dog, cat, bird]
          description: Pet type
      required:
        - id
        - name
        - type
    PetInput:
      type: object
      properties:
        name:
          type: string
        type:
          type: string
          enum: [dog, cat, bird]
      required:
        - name
        - type
```

### Step 2: Run Code Generation

#### Maven
```bash
mvn clean generate-sources
```

#### Gradle
```bash
gradle build
```

This generates:
- API interface in `target/generated-sources/openapi/` (Maven) or `build/generated/openapi/` (Gradle)
- Model classes with validation annotations
- Spring Boot integration code

### Step 3: Implement the Generated API

Create your implementation class:

```java
package com.example.petstore;

import org.springframework.web.bind.annotation.RestController;
import com.example.petstore.api.PetsApi;
import com.example.petstore.model.Pet;
import com.example.petstore.model.PetInput;
import java.util.List;

@RestController
public class PetStoreController implements PetsApi {

  @Override
  public ResponseEntity<List<Pet>> getPets() {
    // Return list of pets
    return ResponseEntity.ok(List.of(
      new Pet().id(1).name("Fluffy").type("cat"),
      new Pet().id(2).name("Rex").type("dog")
    ));
  }

  @Override
  public ResponseEntity<Pet> createPet(PetInput petInput) {
    // Create new pet
    Pet pet = new Pet()
      .id(3)
      .name(petInput.getName())
      .type(petInput.getType());
    return ResponseEntity.status(201).body(pet);
  }

  @Override
  public ResponseEntity<Pet> getPetById(Integer petId) {
    // Get pet by ID
    if (petId == 1) {
      return ResponseEntity.ok(new Pet().id(1).name("Fluffy").type("cat"));
    }
    return ResponseEntity.notFound().build();
  }
}
```

### Step 4: Run Your Application

```bash
# Maven
mvn spring-boot:run

# Gradle
gradle bootRun
```

Access your API:
- GET `http://localhost:8080/pets` — Get all pets
- POST `http://localhost:8080/pets` — Create new pet
- GET `http://localhost:8080/pets/1` — Get pet by ID

## Working with AsyncAPI

For message-based APIs (Kafka, RabbitMQ), use AsyncAPI instead:

```yaml
asyncapi: 2.6.0
info:
  title: Pet Events API
  version: 1.0.0
channels:
  pet-events:
    publish:
      operationId: publishPetEvent
      message:
        payload:
          $ref: '#/components/schemas/PetEvent'
    subscribe:
      operationId: handlePetEvent
      message:
        payload:
          $ref: '#/components/schemas/PetEvent'

components:
  schemas:
    PetEvent:
      type: object
      properties:
        eventId:
          type: string
        petId:
          type: integer
        eventType:
          type: string
          enum: [created, updated, deleted]
```

Generate with:
```bash
# Maven
mvn scs-multiapi:asyncapi-generation

# Gradle - update build.gradle to use asyncapimodel instead
```

## Common Configuration Options

### Output Paths
```xml
<!-- Maven -->
<configuration>
  <generationOutputPath>${project.basedir}/target/generated-sources</generationOutputPath>
</configuration>
```

### Package Names
```xml
<configuration>
  <apiPackage>com.example.api</apiPackage>
  <modelPackage>com.example.model</modelPackage>
</configuration>
```

### Lombok Support
```xml
<configuration>
  <useLombok>true</useLombok>
</configuration>
```

### Jackson Version (for Spring Boot 4.x)
```xml
<configuration>
  <jacksonVersion>3</jacksonVersion>
</configuration>
```

See the [README](https://github.com/sngular/scs-multiapi-plugin/blob/main/README.md) for all options.

## Next Steps

- 📖 Read the [README](https://github.com/sngular/scs-multiapi-plugin/blob/main/README.md) for every configuration option of both goals
- 🔗 See [Loading specs](LOADING_SPECS) to read contracts from a published artifact, a URL or a registry
- ⚡ Explore [Spring-Kafka Integration](SPRING_KAFKA_INTEGRATION) for event-driven systems
- 🧭 Browse [Additional Information](Additional-Information) for the full index

## Troubleshooting

### Build fails with "Cannot find resource"
Ensure your spec file path is correct and the file exists.

### Generated classes not in IDE
Rebuild the project: `mvn clean generate-sources` or `gradle clean build`

### Validation errors in generated classes
Check your OpenAPI spec follows OpenAPI 3.0+ standard.

### Spring Boot integration issues
See the FAQ in [Additional Information](Additional-Information) for more help.

---

**Ready to get started?** [Back to Home](Home) | [View All Guides](Additional-Information)
