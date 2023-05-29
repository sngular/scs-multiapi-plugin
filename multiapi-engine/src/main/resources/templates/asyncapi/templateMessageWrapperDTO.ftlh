package ${packageModel}.${method.parentPackage};

import ${method.classNamespace}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>;

public class ${method.className}MessageDTO {

  private ${method.className} payload;

  private ${method.keyClassName} key;

  public ${method.className}MessageDTO(final ${method.className} payload, final ${method.keyClassName} key) {
    this.payload = payload;
    this.key = key;
  }

  public ${method.className} getPayload() {
   return payload;
  }

  public void setPayload(${method.className} payload) {
    this.payload = payload;
  }

  public ${method.keyClassName} getKey() {
    return key;
  }

  public void setKey(${method.keyClassName} key) {
    this.key = key;
  }

}