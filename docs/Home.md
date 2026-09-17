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
- ✅ Dependency spec loading for composition
- ✅ Multi-file OpenAPI/AsyncAPI support

## Quick Navigation

- **[Getting Started](Getting-Started)** — Installation and first API
- **[OpenAPI Guide](OpenAPI-Guide)** — REST API generation guide
- **[AsyncAPI Guide](AsyncAPI-Guide)** — Message producer/consumer guide
- **[Spring-Kafka Integration](Spring-Kafka-Integration)** — Spring Cloud Stream + Kafka
- **[Configuration Reference](Configuration-Reference)** — All plugin options
- **[Troubleshooting](Troubleshooting)** — Common issues and solutions

## Quick Links

- [Maven Plugin](https://mvnrepository.com/artifact/com.sngular/scs-multiapi-maven-plugin)
- [Gradle Plugin](https://plugins.gradle.org/plugin/com.sngular.scs-multiapi-gradle-plugin)
- [GitHub Repository](https://github.com/sngular/scs-multiapi-plugin)
- [Issues & Feature Requests](https://github.com/sngular/scs-multiapi-plugin/issues)

## Current Version

**Latest**: 7.1.0

### Installation

#### Maven
```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>7.1.0</version>
</plugin>
```

#### Gradle
```groovy
plugins {
  id 'com.sngular.scs-multiapi-gradle-plugin' version '7.1.0'
}
```

## What's New in 7.1.0

- Dependency spec loading with JAR resolution
- Enhanced multipart/form-data support
- Improved URL-based spec loading
- Better error messages and validation
- Performance improvements

## Getting Help

- 📖 Check the [Getting Started](Getting-Started) guide
- 🔍 Search existing [GitHub Issues](https://github.com/sngular/scs-multiapi-plugin/issues)
- 💬 Ask in [GitHub Discussions](https://github.com/sngular/scs-multiapi-plugin/discussions)
- 🐛 Report bugs with detailed examples

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](https://github.com/sngular/scs-multiapi-plugin/blob/main/CONTRIBUTING.md) for guidelines.

---

**Latest Update**: September 2026 | [View Documentation](Additional-Information)
