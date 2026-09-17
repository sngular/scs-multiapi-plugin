# Wiki Structure Plan for SCS MultiAPI Plugin

## Overview

This document outlines the proposed structure for the plugin's wiki documentation, following the mandatory template requirements (Overview, Getting Started, Additional Information).

---

## 1. Home / Overview Page

**Purpose**: Landing page introducing the plugin and its capabilities

```markdown
# SCS MultiAPI Plugin

Generate Java code from OpenAPI and AsyncAPI specifications automatically.

## What is SCS MultiAPI?

The SCS MultiAPI Plugin is a Maven and Gradle plugin that automates the generation of:
- REST API controllers and models (from OpenAPI specs)
- Message producers and consumers (from AsyncAPI specs)
- Validation and serialization code
- Spring Boot integration code

## Key Features

- ✅ OpenAPI 3.0+ support
- ✅ AsyncAPI 2.x support  
- ✅ Maven and Gradle plugins
- ✅ Spring Boot 2.x, 3.x, 4.x support
- ✅ Spring Cloud Stream integration
- ✅ Kafka, RabbitMQ support (via bindings)
- ✅ Jackson 2 & Jackson 3 (Spring Boot 4)
- ✅ Lombok support
- ✅ Custom validators
- ✅ Reactive types (Mono/Flux)
- ✅ Load specs from classpath, filesystem, HTTP, Apicurio Registry

## Quick Links

- [Getting Started →](#getting-started)
- [OpenAPI Guide →](#openapi-guide)
- [AsyncAPI Guide →](#asyncapi-guide)
- [Spring-Kafka Integration →](#spring-kafka)
- [Configuration Reference →](#configuration)

## Latest Version

**Current**: 7.0.0
- Maven: `com.sngular:scs-multiapi-maven-plugin:7.0.0`
- Gradle: `com.sngular.scs-multiapi-gradle-plugin` version `7.0.0`
```

---

## 2. Getting Started Page

**Purpose**: Step-by-step guide for first-time users

```markdown
# Getting Started

## Prerequisites

- Java 11+ (Java 21 recommended)
- Maven 3.9.6+ OR Gradle 7.0+
- Spring Boot 2.x, 3.x, or 4.x

## Installation

### Maven

Add to your `pom.xml`:

```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>7.0.0</version>
  <executions>
    <execution>
      <phase>generate-sources</phase>
      <goals>
        <goal>openapi-generation</goal>
        <!-- OR <goal>asyncapi-generation</goal> -->
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

### Gradle

Add to your `build.gradle`:

```groovy
plugins {
  id 'com.sngular.scs-multiapi-gradle-plugin' version '7.0.0'
}

dependencies {
  implementation 'io.swagger.parser.v3:swagger-parser-core:2.1.20'
  implementation 'io.swagger.core.v3:swagger-annotations-jakarta:2.2.20'
  implementation 'jakarta.validation:jakarta.validation-api:3.0.2'
}

asyncapimodel {  // or openapimodel for OpenAPI
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
paths:
  /pets:
    get:
      operationId: getPets
      responses:
        '200':
          description: List of pets
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/Pet'
components:
  schemas:
    Pet:
      type: object
      properties:
        id:
          type: integer
        name:
          type: string
      required:
        - id
        - name
```

### Step 2: Run Generation

```bash
# Maven
mvn clean generate-sources

# Gradle
gradle build
```

### Step 3: Implement Generated Code

The plugin generates:
- API interface (implement this)
- Model classes (use these)
- Validation annotations (automatic)

### Step 4: Deploy

Your Spring Boot app now exposes the REST API endpoints defined in the spec.

## Next Steps

- [Configure OpenAPI generation →](./openapi-guide)
- [Configure AsyncAPI generation →](./asyncapi-guide)
- [Explore advanced configuration →](./configuration)
- [View examples →](./examples)
```

---

## 3. Additional Information / Advanced Topics

**Purpose**: Detailed guides and reference material

```markdown
# Additional Information

## Guides

### OpenAPI & REST APIs
- [OpenAPI Configuration Guide](./openapi-configuration)
- [Model Generation Options](./model-generation)
- [REST Client Generation](./rest-client)
- [Reactive Types Support](./reactive-types)
- [Spring Boot 4 & Jackson 3](./spring-boot-4)

### AsyncAPI & Messaging
- [AsyncAPI Configuration Guide](./asyncapi-configuration)
- [Spring-Kafka Integration](./spring-kafka-integration)
- [RabbitMQ Integration](./rabbitmq-integration)
- [Spring Cloud Stream Setup](./spring-cloud-stream)
- [Pact Contract Testing](./pact-testing)

### Advanced Features
- [Loading Specs from Dependencies](./classpath-loading)
- [Remote Spec Loading (HTTP/Apicurio)](./remote-specs)
- [Custom Validators](./custom-validators)
- [Lombok Annotation Support](./lombok)
- [Springwolf Annotations](./springwolf)

### Development & Testing
- [Testing Generated Code](./testing)
- [Migration Guides](./migration)
- [Troubleshooting](./troubleshooting)
- [Contributing](./contributing)

## Reference

### Configuration Options
- [Maven Plugin Configuration](./maven-configuration)
- [Gradle Plugin Configuration](./gradle-configuration)
- [Common Options](./common-configuration)

### Examples
- [Pet Store API Example](./examples/petstore)
- [Kafka Event Processing](./examples/kafka-events)
- [Multi-API Project](./examples/multi-api)

## FAQ

Q: How do I handle model inheritance (allOf, oneOf, anyOf)?
A: See [Composition Support](./composition-support)

Q: Can I use different generators for different specs?
A: Yes, see [Multi-API Configuration](./multi-api-configuration)

Q: How do I update generated code without overwriting customizations?
A: Use the `overWriteModel` option and implement interfaces for logic.

## Support

- [GitHub Issues](https://github.com/sngular/scs-multiapi-plugin/issues)
- [GitHub Discussions](https://github.com/sngular/scs-multiapi-plugin/discussions)
- [Contributing Guidelines](./CONTRIBUTING)
```

---

## 4. Updated README

**Purpose**: Quick reference, links to wiki (keep short, <2KB)

```markdown
# SCS MultiAPI Plugin

[![Codacy Badge](https://app.codacy.com/project/badge/Grade/4a9be5a4b6ab48afba293b2315edd47e)](https://app.codacy.com/gh/sngular/scs-multiapi-plugin/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)[![Maven Central](https://img.shields.io/maven-central/v/com.sngular/scs-multiapi-maven-plugin.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.sngular%22%20AND%20a:%22scs-multiapi-maven-plugin%22)

Automatically generate Java code from OpenAPI and AsyncAPI specifications.

## Features

- **OpenAPI 3.0+**: Generate REST APIs with Spring Boot
- **AsyncAPI 2.x**: Generate message producers/consumers with Spring Cloud Stream
- **Maven & Gradle**: Native plugins for both build systems
- **Spring Boot 2/3/4**: Full support including Jackson 3 (Spring Boot 4)
- **Kafka & RabbitMQ**: Message broker support via protocol bindings
- **Remote Specs**: Load from HTTP, Apicurio Registry, or classpath dependencies
- **Reactive Types**: Mono/Flux support for reactive endpoints

## Quick Start

### Maven
```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>7.0.0</version>
  <executions>
    <execution>
      <goals>
        <goal>openapi-generation</goal>
      </goals>
      <configuration>
        <specFiles>
          <specFile>
            <filePath>src/main/resources/api.yml</filePath>
          </specFile>
        </specFiles>
      </configuration>
    </execution>
  </executions>
</plugin>
```

### Gradle
```groovy
plugins {
  id 'com.sngular.scs-multiapi-gradle-plugin' version '7.0.0'
}

openapimodel {
  specFile { filePath = 'src/main/resources/api.yml' }
  overWriteModel = true
}
```

Then run: `mvn clean generate-sources` (Maven) or `gradle build` (Gradle)

## Documentation

Full documentation is available in the [Wiki](https://github.com/sngular/scs-multiapi-plugin/wiki):

- **[Getting Started](https://github.com/sngular/scs-multiapi-plugin/wiki/Getting-Started)** - Installation and first steps
- **[OpenAPI Guide](https://github.com/sngular/scs-multiapi-plugin/wiki/OpenAPI-Configuration)** - REST API generation
- **[AsyncAPI Guide](https://github.com/sngular/scs-multiapi-plugin/wiki/AsyncAPI-Configuration)** - Message producer/consumer generation
- **[Spring-Kafka Integration](https://github.com/sngular/scs-multiapi-plugin/wiki/Spring-Kafka-Integration)** - Complete Kafka setup guide
- **[Configuration Reference](https://github.com/sngular/scs-multiapi-plugin/wiki/Configuration-Reference)** - All options explained

## Examples

See the [examples directory](./examples) for:
- Pet Store REST API
- Kafka event processing
- Multi-API projects with both OpenAPI and AsyncAPI

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md)

## License

[License info here]

## Support

- 📖 [Wiki & Guides](https://github.com/sngular/scs-multiapi-plugin/wiki)
- 🐛 [Report Issues](https://github.com/sngular/scs-multiapi-plugin/issues)
- 💬 [Discussions](https://github.com/sngular/scs-multiapi-plugin/discussions)
```

---

## Migration Timeline

### Phase 1 (This Sprint)
- ✅ Create Overview page
- ✅ Create Getting Started page
- ✅ Create Additional Information page
- ✅ Publish to wiki

### Phase 2 (Next Sprint)
- Move detailed sections from current README to wiki
- Create specialized guides (Kafka, RabbitMQ, Lombok, etc.)
- Add examples

### Phase 3 (Following Sprint)
- Create new streamlined README (2KB)
- Archive old README
- Update all external links to point to wiki

---

## File Organization

```
wiki/
├── Home.md                          (Overview)
├── Getting-Started.md
├── Additional-Information.md
├── openapi-configuration.md
├── asyncapi-configuration.md
├── spring-kafka-integration.md
├── spring-cloud-stream.md
├── rest-client-generation.md
├── custom-validators.md
├── gradle-configuration.md
├── maven-configuration.md
├── examples/
│   ├── petstore-api.md
│   ├── kafka-events.md
│   └── multi-api.md
├── troubleshooting.md
└── faq.md

repo-root/
└── README.md (streamlined, 2KB, links to wiki)
```

---

## Notes

- All wiki pages should include:
  - Clear title
  - Table of contents for pages >1000 words
  - Code examples (Maven + Gradle when applicable)
  - Links to related topics
  - "Back to main" link

- Maintain current README.md in repo for quick reference
- Move detailed content (>500 lines) to wiki pages
- Keep examples in repo and reference from wiki
