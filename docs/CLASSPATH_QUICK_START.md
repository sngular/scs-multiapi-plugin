# Classpath Spec Loading - Quick Start Guide

## TL;DR

Load API specs from dependency JARs by specifying the classpath path instead of a filesystem path.

```xml
<!-- Maven: Add dependency with your API spec -->
<dependency>
  <groupId>com.mycompany</groupId>
  <artifactId>api-specs</artifactId>
  <version>1.0.0</version>
</dependency>

<!-- Maven: Reference the spec in plugin configuration -->
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <configuration>
    <specFile>
      <filePath>com/mycompany/api/openapi.yml</filePath>
      <!-- ^^ Classpath path, NOT filesystem path -->
    </specFile>
  </configuration>
</plugin>
```

```gradle
// Gradle: Add dependency
dependencies {
  implementation 'com.mycompany:api-specs:1.0.0'
}

// Gradle: Reference in plugin
multiapi {
  openApiSpec {
    filePath = 'com/mycompany/api/openapi.yml'
    apiPackage = 'com.mycompany.api'
  }
}
```

---

## How to Create an API Specs Dependency

### Step 1: Create Maven Module

```
api-specs/
├── pom.xml
└── src/
    └── main/
        └── resources/
            └── com/
                └── mycompany/
                    └── api/
                        ├── openapi.yml
                        └── schemas/
                            ├── user.yml
                            └── common.yml
```

### Step 2: Configure pom.xml

```xml
<project>
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.mycompany</groupId>
  <artifactId>api-specs</artifactId>
  <version>1.0.0</version>
  <packaging>jar</packaging>
  
  <build>
    <resources>
      <resource>
        <directory>src/main/resources</directory>
        <includes>
          <include>com/mycompany/api/**</include>
        </includes>
      </resource>
    </resources>
  </build>
</project>
```

### Step 3: Create Your Specs

```yaml
# src/main/resources/com/mycompany/api/openapi.yml
openapi: 3.0.0
info:
  title: My API
  version: 1.0.0

paths:
  /users:
    get:
      responses:
        '200':
          description: List of users
          content:
            application/json:
              schema:
                $ref: schemas/user.yml
```

```yaml
# src/main/resources/com/mycompany/api/schemas/user.yml
type: object
properties:
  id:
    type: integer
  name:
    type: string
```

### Step 4: Build and Deploy

```bash
cd api-specs
mvn clean install

# Now available in your local repository and CI/CD
```

---

## Multiple Specs from Multiple Dependencies

```xml
<dependencies>
  <dependency>
    <groupId>com.mycompany</groupId>
    <artifactId>user-api-specs</artifactId>
    <version>1.0.0</version>
  </dependency>
  <dependency>
    <groupId>com.mycompany</groupId>
    <artifactId>order-api-specs</artifactId>
    <version>2.0.0</version>
  </dependency>
</dependencies>

<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <configuration>
    <specFiles>
      <!-- From user-api-specs JAR -->
      <specFile>
        <filePath>com/mycompany/user/openapi.yml</filePath>
        <apiPackage>com.mycompany.user.api</apiPackage>
      </specFile>
      
      <!-- From order-api-specs JAR -->
      <specFile>
        <filePath>com/mycompany/order/openapi.yml</filePath>
        <apiPackage>com.mycompany.order.api</apiPackage>
      </specFile>
    </specFiles>
  </configuration>
</plugin>
```

---

## File Resolution Order

The plugin searches for specs in this order:

1. **Remote URL** → Direct download (http://, https://, ftp://)
2. **Classpath** → All JARs in dependencies + resources
3. **Filesystem** → Relative to project root

```
filePath = "com/mycompany/api/openapi.yml"

Check 1: Is it a URL? NO
  ↓
Check 2: Is it in classpath (any JAR)?
  YES → Load from JAR ✓
  ↓
Check 3: Try as filesystem path
  If not found: FileNotFoundException
```

---

## Troubleshooting

### "FileNotFoundException: Could not find YAML file"

1. **Verify dependency is included**:
   ```bash
   mvn dependency:tree | grep api-specs
   ```

2. **Check spec exists in JAR**:
   ```bash
   unzip -l ~/.m2/repository/com/mycompany/api-specs/1.0.0/api-specs-1.0.0.jar
   # Look for: com/mycompany/api/openapi.yml
   ```

3. **Verify pom.xml resource config**:
   ```xml
   <resources>
     <resource>
       <directory>src/main/resources</directory>
     </resource>
   </resources>
   ```

### "Could not resolve $ref: schemas/user.yml"

1. **Check reference file exists in JAR**:
   ```bash
   unzip -l myjar.jar | grep user.yml
   ```

2. **Verify reference is relative** (not absolute):
   ```yaml
   # ✓ Correct: Relative path
   $ref: schemas/user.yml
   
   # ✗ Wrong: Absolute path
   $ref: /schemas/user.yml
   ```

3. **Enable debug logging**:
   ```bash
   mvn clean generate-sources -Dorg.slf4j.simpleLogger.defaultLogLevel=debug
   ```

---

## Working Examples

### Example 1: Simple Spec in JAR

**api-specs dependency JAR**:
```
openapi.yml
└── com/example/api/
    └── openapi.yml
```

**Your project**:
```xml
<filePath>com/example/api/openapi.yml</filePath>
```

### Example 2: Spec with References

**api-specs dependency JAR**:
```
├── com/example/api/openapi.yml
└── schemas/
    ├── user.yml
    └── common.yml
```

**openapi.yml references**:
```yaml
$ref: schemas/user.yml
$ref: schemas/common.yml
```

**Your project**:
```xml
<filePath>com/example/api/openapi.yml</filePath>
<!-- References automatically resolved from JAR -->
```

### Example 3: Multiple Dependencies

**user-specs JAR**:
```
└── api/user/openapi.yml
```

**order-specs JAR**:
```
└── api/order/openapi.yml
```

**local specs**:
```
└── specs/local.yml
```

**Your project**:
```xml
<specFiles>
  <specFile>
    <filePath>api/user/openapi.yml</filePath>
    <apiPackage>com.mycompany.user</apiPackage>
  </specFile>
  <specFile>
    <filePath>api/order/openapi.yml</filePath>
    <apiPackage>com.mycompany.order</apiPackage>
  </specFile>
  <specFile>
    <filePath>specs/local.yml</filePath>
    <apiPackage>com.mycompany.local</apiPackage>
  </specFile>
</specFiles>
```

---

## Best Practices

✅ **DO:**
- Use clear, package-like paths: `com/mycompany/api/openapi.yml`
- Create a separate Maven module for API specs
- Version your specs JAR independently
- Use relative references in specs: `$ref: schemas/user.yml`
- Keep specs at the classpath root level when possible

❌ **DON'T:**
- Mix filesystem and classpath paths in same filePath
- Use absolute filesystem paths in classpath specs: `/path/to/file`
- Put specs directly in target classes without resources config
- Use deeply nested directory structures
- Reference files from different packages (use relative paths)

---

## Common Patterns

### Pattern 1: Shared Core Specs

```
All services depend on → shared-api-specs → Contains base definitions
                      ↓
                    your-service → Generates local API code
```

### Pattern 2: API Versioning

```
api-specs-v1 JAR → Contains v1 of API
api-specs-v2 JAR → Contains v2 of API

Service chooses which version to use
```

### Pattern 3: Composite APIs

```
foundation-specs (commons, pagination)
    ↑
    ├─→ user-service (depends on foundation)
    ├─→ order-service (depends on foundation)
    └─→ payment-service (depends on foundation)
```

---

## More Information

- **Full Documentation**: `docs/CLASSPATH_SPEC_LOADING.md`
- **Configuration Guide**: Plugin configuration reference
- **Issue #426**: Implementation details and design

---

## Quick Checklist

Before using classpath specs:

- [ ] Created `api-specs` Maven module
- [ ] Added resources in `src/main/resources/`
- [ ] Configured `<resources>` in pom.xml
- [ ] Built and deployed with `mvn clean install`
- [ ] Added dependency to your project
- [ ] Updated plugin `<filePath>` to classpath path
- [ ] Verified spec references are relative paths
- [ ] Run `mvn clean generate-sources` to test

---

**Ready to use!** 🚀
