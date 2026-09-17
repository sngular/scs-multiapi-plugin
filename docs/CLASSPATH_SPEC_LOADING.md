# Loading API Specifications from Classpath Resources

## Overview

The SCS MultiAPI Plugin supports loading OpenAPI and AsyncAPI specifications from classpath resources, including API specifications bundled in dependency JARs. This feature enables you to:

- Store API specs in shared libraries/dependencies
- Reuse specs across multiple projects
- Manage specs in separate repositories
- Resolve external references (`$ref`) within dependency JARs
- Mix specs from multiple dependencies, the filesystem, and remote URLs

## Table of Contents

1. [Basic Usage](#basic-usage)
2. [File Resolution Strategy](#file-resolution-strategy)
3. [Multiple Dependencies](#multiple-dependencies)
4. [External References](#external-references)
5. [Configuration Examples](#configuration-examples)
6. [Troubleshooting](#troubleshooting)

---

## Basic Usage

### Loading from Classpath

Specify a spec path without a filesystem prefix. The plugin will search:

1. **Classpath resources** (including dependency JARs)
2. **Filesystem** relative to the project
3. **Remote URLs** (if protocol specified)

#### Example: Maven Configuration

```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>7.0.0</version>
  <configuration>
    <specFile>
      <filePath>openapi.yml</filePath>
      <!-- Will search in classpath first, then filesystem -->
    </specFile>
  </configuration>
  <dependencies>
    <!-- Your API spec is in this dependency -->
    <dependency>
      <groupId>com.mycompany</groupId>
      <artifactId>api-specs</artifactId>
      <version>1.0.0</version>
    </dependency>
  </dependencies>
</plugin>
```

#### Example: Gradle Configuration

```gradle
plugins {
  id 'com.sngular.multiapi.plugin' version '7.0.0'
}

multiapi {
  openApiSpec {
    filePath = 'openapi.yml'  // Searches classpath first
  }
}

dependencies {
  implementation 'com.mycompany:api-specs:1.0.0'
}
```

### Loading from Subdirectories

Classpath resources are resolved in package-like paths:

```
dependency-jar/
├── com/
│   └── mycompany/
│       └── api/
│           ├── openapi.yml
│           └── fragments.yml
```

**Configuration:**
```xml
<filePath>com/mycompany/api/openapi.yml</filePath>
```

---

## File Resolution Strategy

The plugin uses a **three-tier resolution strategy** to locate spec files:

### Tier 1: Remote URLs (Highest Priority)

Files matching remote URL patterns are fetched directly:
- `http://...`
- `https://...`
- `ftp://...`

**Example:**
```xml
<filePath>https://api.example.com/openapi.yml</filePath>
```

### Tier 2: Classpath Resources

The plugin searches for the file in the classpath:
- Maven dependencies
- JAR files on the classpath
- Resources in the project itself

**How it works:**
```
ClassLoader.getResource("com/mycompany/api/openapi.yml")
```

**Benefits:**
- Works with any JAR in the classpath
- Supports nested directories
- Correctly resolves relative references within JARs

### Tier 3: Filesystem (Fallback)

If the file is not found in classpath, it's resolved as a filesystem path:
- **Absolute paths**: Resolved directly
  ```xml
  <filePath>/usr/share/specs/openapi.yml</filePath>
  ```
- **Relative paths**: Resolved relative to the project root
  ```xml
  <filePath>specs/openapi.yml</filePath>
  ```

### Resolution Priority Summary

```
request: filePath="openapi.yml"
    ↓
[Check 1] Is it a remote URL? (http://, https://, ftp://)
    → YES: Fetch from URL, DONE
    → NO: Continue
    ↓
[Check 2] Is it in the classpath?
    → YES: Load from JAR/classpath, DONE
    → NO: Continue
    ↓
[Check 3] Is it a filesystem path?
    → YES: Load from filesystem, DONE
    → NO: Throw FileNotFoundException
```

---

## Multiple Dependencies

### Scenario: Specs from Multiple Dependencies

When your project depends on multiple JARs containing API specs:

```
project/
├── pom.xml
└── specs/
    └── local-api.yml (local spec)

dependencies:
├── api-specs-core:1.0.0        (contains: core/openapi.yml)
├── api-specs-extensions:2.0.0  (contains: extensions/asyncapi.yml)
└── api-specs-models:1.0.0      (contains: models/schemas.yml)
```

**Configuration:**

```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <configuration>
    <!-- Multiple specs from different sources -->
    <specFiles>
      <!-- From dependency: api-specs-core -->
      <specFile>
        <filePath>core/openapi.yml</filePath>
        <apiPackage>com.mycompany.api.core</apiPackage>
      </specFile>

      <!-- From dependency: api-specs-extensions -->
      <specFile>
        <filePath>extensions/asyncapi.yml</filePath>
        <apiPackage>com.mycompany.api.extensions</apiPackage>
      </specFile>

      <!-- From local filesystem -->
      <specFile>
        <filePath>specs/local-api.yml</filePath>
        <apiPackage>com.mycompany.api.local</apiPackage>
      </specFile>
    </specFiles>
  </configuration>
</plugin>
```

### Dependency JAR Structure

Your dependency JAR should be structured like:

```
api-specs-core-1.0.0.jar
├── core/
│   ├── openapi.yml
│   └── schemas/
│       ├── common.yml
│       └── domain.yml
└── META-INF/
```

**Create the JAR with Maven:**

```xml
<!-- In your api-specs-core pom.xml -->
<project>
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.mycompany</groupId>
  <artifactId>api-specs-core</artifactId>
  <version>1.0.0</version>
  <packaging>jar</packaging>

  <build>
    <resources>
      <resource>
        <directory>src/main/resources</directory>
        <includes>
          <include>core/**/*.yml</include>
        </includes>
      </resource>
    </resources>
  </build>
</project>
```

**Directory structure in api-specs-core:**

```
api-specs-core/
├── pom.xml
└── src/
    └── main/
        └── resources/
            └── core/
                ├── openapi.yml
                └── schemas/
                    ├── common.yml
                    └── domain.yml
```

---

## External References

### What are External References?

API specs often reference schemas or path definitions in separate files using `$ref`:

```yaml
paths:
  /users:
    get:
      responses:
        200:
          description: List of users
          content:
            application/json:
              schema:
                $ref: schemas/user.yml

  /users/{id}:
    $ref: paths/user-details.yml
```

### How They're Resolved

When a spec is loaded from a **classpath resource**, external references are **correctly resolved relative to the JAR location**:

```
core-api.jar
├── openapi.yml         ← Main spec file
├── schemas/
│   ├── user.yml        ← Referenced by $ref: schemas/user.yml
│   └── address.yml
└── paths/
    └── user-details.yml ← Referenced by $ref: paths/user-details.yml
```

When `openapi.yml` is loaded from the JAR, all external refs are resolved **within that same JAR directory**.

### Nested Dependencies (Advanced)

A spec can reference files in different dependency packages:

```yaml
# In dependency A (core-api.jar)
components:
  schemas:
    User:
      $ref: ../shared-models/schemas/base-user.yml
```

This is supported because the resolution is based on **URI resolution** which properly handles JAR paths:

```
Step 1: Load core-api.jar!/api/openapi.yml
        → Base URI: jar:file:/m2/core-api-1.0.jar!/api/

Step 2: Resolve $ref: ../shared-models/schemas/base-user.yml
        → jar:file:/m2/core-api-1.0.jar!/api/
        → Resolve ../ → jar:file:/m2/core-api-1.0.jar!/
        → Resolve shared-models/schemas/base-user.yml → jar:file:/m2/core-api-1.0.jar!/shared-models/schemas/base-user.yml

Step 3: If shared-models is in a different JAR:
        → ClassLoader searches and finds: shared-models-2.0.jar!/schemas/base-user.yml
```

---

## Configuration Examples

### Example 1: Microservices with Shared Core API

**Shared API Specs Repository** (published as `shared-api-specs` JAR):

```
shared-api-specs/
├── pom.xml
└── src/main/resources/
    └── api/
        ├── openapi.yml (base API)
        └── schemas/
            ├── pagination.yml
            └── errors.yml
```

**User Service** (depends on shared-api-specs):

```xml
<!-- user-service/pom.xml -->
<dependency>
  <groupId>com.mycompany</groupId>
  <artifactId>shared-api-specs</artifactId>
  <version>1.0.0</version>
</dependency>

<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <configuration>
    <specFile>
      <filePath>api/openapi.yml</filePath>
      <apiPackage>com.mycompany.user.api</apiPackage>
      <modelPackage>com.mycompany.user.model</modelPackage>
    </specFile>
  </configuration>
</plugin>
```

**Order Service** (depends on same shared-api-specs):

```xml
<!-- order-service/pom.xml -->
<dependency>
  <groupId>com.mycompany</groupId>
  <artifactId>shared-api-specs</artifactId>
  <version>1.0.0</version>
</dependency>

<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <configuration>
    <specFile>
      <filePath>api/openapi.yml</filePath>
      <apiPackage>com.mycompany.order.api</apiPackage>
      <modelPackage>com.mycompany.order.model</modelPackage>
    </specFile>
  </configuration>
</plugin>
```

### Example 2: Layered Dependencies

Three-tier dependency structure:

```
foundation-api (base schemas and definitions)
    ↑
    ├─── core-api (business domain)
    │        ↑
    │        └─── my-service (consumes both)
    │
    └─── other-service (also consumes foundation-api)
```

**foundation-api JAR:**
```
foundation-api/
└── schemas/
    ├── common.yml
    ├── errors.yml
    └── pagination.yml
```

**core-api JAR:**
```
core-api/
├── openapi.yml (references: $ref: ../../foundation/schemas/common.yml)
└── paths/
    └── users.yml
```

**my-service:**
```xml
<dependencies>
  <dependency>
    <groupId>com.mycompany</groupId>
    <artifactId>core-api</artifactId>
    <version>2.0.0</version>
  </dependency>
  <!-- core-api already depends on foundation-api -->
</dependencies>

<plugin>
  <configuration>
    <specFile>
      <filePath>openapi.yml</filePath>
    </specFile>
  </configuration>
</plugin>
```

### Example 3: Mix Local and Remote Specs

```xml
<configuration>
  <specFiles>
    <!-- Local spec in project -->
    <specFile>
      <filePath>src/main/resources/local-api.yml</filePath>
      <apiPackage>com.mycompany.local</apiPackage>
    </specFile>

    <!-- From dependency -->
    <specFile>
      <filePath>com/shared/openapi.yml</filePath>
      <apiPackage>com.mycompany.shared</apiPackage>
    </specFile>

    <!-- From remote registry -->
    <specFile>
      <filePath>https://registry.example.com/api/openapi.yml</filePath>
      <apiPackage>com.mycompany.external</apiPackage>
    </specFile>
  </specFiles>
</configuration>
```

---

## Troubleshooting

### Issue: FileNotFoundException - Spec Not Found

**Error:**
```
FileNotFoundException: Could not find YAML file: com/mycompany/api/openapi.yml
```

**Solutions:**

1. **Check Maven dependency includes the spec:**
   ```bash
   # Unzip the JAR and verify file exists
   unzip -l ~/.m2/repository/com/mycompany/api-specs/1.0.0/api-specs-1.0.0.jar | grep openapi.yml
   ```

2. **Verify Maven resources are configured:**
   ```xml
   <build>
     <resources>
       <resource>
         <directory>src/main/resources</directory>
       </resource>
     </resources>
   </build>
   ```

3. **Check path is relative to classpath root:**
   ```
   ✓ com/mycompany/api/openapi.yml
   ✗ /com/mycompany/api/openapi.yml (leading slash)
   ✗ ./com/mycompany/api/openapi.yml (redundant prefix)
   ```

### Issue: External References Not Resolved

**Error:**
```
FileParseException: Could not resolve $ref: schemas/user.yml
```

**Causes and solutions:**

1. **Incorrect reference path:**
   ```yaml
   # ✓ Correct: Relative to current file
   $ref: schemas/user.yml

   # ✗ Wrong: Absolute filesystem path
   $ref: /schemas/user.yml

   # ✗ Wrong: HTTP reference from JAR context
   $ref: http://example.com/schemas/user.yml
   ```

2. **File doesn't exist in JAR:**
   ```bash
   # Verify the file is in the JAR
   unzip -l myjar.jar | grep -E "(openapi|schemas)"
   ```

3. **Using filesystem path for classpath spec:**
   ```xml
   <!-- ✗ Wrong: Mixing classpath and filesystem -->
   <filePath>api/openapi.yml</filePath>  <!-- From classpath -->
   <!-- But referencing:
        $ref: /local/path/schemas.yml  <!-- Won't work -->
   -->
   ```

### Issue: Multiple Versions of Same Spec

**Problem:** Two dependencies have specs with the same name.

**Solution:** Use explicit paths to distinguish:

```xml
<specFiles>
  <!-- Use full path to avoid collision -->
  <specFile>
    <filePath>com/vendor1/api-v1/openapi.yml</filePath>
    <apiPackage>com.mycompany.vendor1</apiPackage>
  </specFile>

  <specFile>
    <filePath>com/vendor2/api-v2/openapi.yml</filePath>
    <apiPackage>com.mycompany.vendor2</apiPackage>
  </specFile>
</specFiles>
```

### Issue: Spec Works Locally But Not in CI/CD

**Cause:** Dependency not included in build.

**Solution:**

1. **Verify dependency in pom.xml:**
   ```bash
   mvn dependency:tree | grep api-specs
   ```

2. **In CI/CD, ensure dependency is resolved:**
   ```bash
   mvn clean install -U  # Update dependencies
   ```

3. **Check plugin configuration:**
   - Dependency must be listed **before** or **within** the plugin's `<dependencies>` section
   - Some CI/CD systems need explicit resolution

---

## Advanced Topics

### Custom ClassLoader

If your specs are loaded via a custom ClassLoader, ensure it's properly configured in your Maven/Gradle build:

```xml
<plugin>
  <!-- Plugin settings -->
</plugin>

<!-- Add classpath extension if needed -->
<dependencies>
  <dependency>
    <groupId>your.custom</groupId>
    <artifactId>classloader-lib</artifactId>
  </dependency>
</dependencies>
```

### Performance Considerations

- **Classpath lookup** is fast (single resource resolution)
- **External references** are resolved sequentially (one ref at a time)
- **Large dependency trees** may slow down resolution due to JAR scanning

**Optimization:**
- Keep spec files at the classpath root if possible
- Avoid deeply nested directories
- Use explicit package-qualified paths

---

## Implementation Details

### How Classpath Resolution Works

```java
// Step 1: Get resource from ClassLoader
URL resource = ClassLoader.getResource("com/mycompany/api/openapi.yml");

// Step 2: Convert to URI (preserves JAR path)
URI resourceUri = resource.toURI();
// Result: jar:file:/path/to/api-specs-1.0.0.jar!/com/mycompany/api/openapi.yml

// Step 3: Extract parent directory
URI parentUri = FileLocationUtil.getParentUri(resourceUri);
// Result: jar:file:/path/to/api-specs-1.0.0.jar!/com/mycompany/api/

// Step 4: Resolve external references relative to parent
URI resolved = parentUri.resolve("schemas/user.yml");
// Result: jar:file:/path/to/api-specs-1.0.0.jar!/com/mycompany/api/schemas/user.yml
```

### URI Schemes

- **file://** - Filesystem paths
- **jar:** - JAR entries (format: `jar:file:/path/to/lib.jar!/entry/path`)
- **http://, https://** - Remote URLs

---

## Changelog

### Version 7.0.0+ (This Implementation)

- ✅ Fixed AsyncApiGenerator classpath URI handling
- ✅ Fixed OpenApiGenerator to detect actual spec location
- ✅ Fixed SchemaUtil to handle JAR URIs for external refs
- ✅ Added FileLocationUtil shared utility
- ✅ Support for multiple dependency scenarios

### Version 7.0.0 and Earlier

- Had issues with external references in classpath specs
- Assumed specs were always on filesystem
- Could not properly resolve JAR-relative paths

---

## See Also

- [Plugin Configuration Guide](./CONFIGURATION.md)
- [AsyncAPI Support](./ASYNCAPI.md)
- [OpenAPI Support](./OPENAPI.md)
