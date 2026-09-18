# SCS MultiAPI Plugin

Generate Java code from OpenAPI and AsyncAPI specifications automatically.

## What is SCS MultiAPI?

The SCS MultiAPI Plugin is a Maven and Gradle plugin that automates the generation of:
- REST API controllers and models (from OpenAPI specs)
- Message producers and consumers (from AsyncAPI specs)
- Validation and serialization code
- Spring Boot integration code

## Key Features

- ✅ OpenAPI 3.0+ support with extensions
- ✅ AsyncAPI 2.x support with Spring Cloud Stream
- ✅ Maven and Gradle plugins
- ✅ Spring Boot 2.x, 3.x, 4.x support
- ✅ Spring Cloud Stream integration
- ✅ Kafka, RabbitMQ support (via bindings)
- ✅ Jackson 2 & Jackson 3 (Spring Boot 4)
- ✅ Lombok support
- ✅ Custom validators
- ✅ Reactive types (Mono/Flux)
- ✅ Load specs from classpath, filesystem, HTTP, Apicurio Registry
- ✅ Contracts loaded from a published artifact by coordinates
- ✅ Multi-file OpenAPI/AsyncAPI support

## Quick Navigation

- **[Getting Started](Getting-Started)** — Installation and first API
- **[Loading specs](LOADING_SPECS)** — Where a contract can live, and how to
  point at it
- **[Spring-Kafka Integration](SPRING_KAFKA_INTEGRATION)** — Spring Cloud
  Stream + Kafka
- **[Architecture](ARCHITECTURE)** — How generation works, for contributors
- **[Additional Information](Additional-Information)** — Every guide, FAQ and
  support link
- **[Configuration reference][readme]** — All plugin options

[readme]: https://github.com/sngular/scs-multiapi-plugin/blob/main/README.md

## Quick Links

- [Maven Plugin](https://mvnrepository.com/artifact/com.sngular/scs-multiapi-maven-plugin)
- [Gradle Plugin](https://plugins.gradle.org/plugin/com.sngular.scs-multiapi-gradle-plugin)
- [GitHub Repository](https://github.com/sngular/scs-multiapi-plugin)
- [Issues & Feature Requests](https://github.com/sngular/scs-multiapi-plugin/issues)

## Current Version

**Latest**: 7.1.3

### Installation

#### Maven
```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>7.1.3</version>
</plugin>
```

#### Gradle
```groovy
plugins {
  id 'com.sngular.scs-multiapi-gradle-plugin' version '7.1.3'
}
```

## What's New in 7.1.3

- Contracts declared with `fromGroupId`/`fromArtifactId` are resolved from the
  repositories the build is configured with, including private ones, and
  multi-file contracts inside an artifact resolve their references. See
  [Loading specs](LOADING_SPECS).
- The same coordinates now work for AsyncAPI specs, and in Gradle they are set
  per `specFile` rather than on the task.

## Getting Help

- 📖 Check the [Getting Started](Getting-Started) guide
- 🔍 Search existing [GitHub Issues](https://github.com/sngular/scs-multiapi-plugin/issues)
- 💬 Ask in [GitHub Discussions](https://github.com/sngular/scs-multiapi-plugin/discussions)
- 🐛 Report bugs with detailed examples

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](https://github.com/sngular/scs-multiapi-plugin/blob/main/CONTRIBUTING.md) for guidelines.

---

**Latest Update**: September 2026 | [View Documentation](Additional-Information)
