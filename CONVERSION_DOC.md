# OpenAPI to RDF/SHACL Conversion Documentation

This comprehensive documentation explains how OpenAPI YAML specifications are converted to RDF vocabularies and SHACL validation shapes, with detailed examples for each conversion pattern.

## Table of Contents

1. [Overview](#overview)
2. [Basic Data Types](#basic-data-types)
3. [Object Schemas](#object-schemas)
4. [Array Schemas](#array-schemas)
5. [Reference Schemas](#reference-schemas)
6. [Logical Operators](#logical-operators)
7. [String Constraints](#string-constraints)
8. [Numeric Constraints](#numeric-constraints)
9. [OpenAPI-Specific Features](#openapi-specific-features)
10. [Namespace Management](#namespace-management)
11. [Output Structure](#output-structure)

## Overview

The OpenAPI to RDF/SHACL converter generates two separate outputs:

- **RDF Vocabulary** (`*_rdf.ttl`): Contains RDFS classes, properties, and their relationships
- **SHACL Shapes** (`*_shacl.ttl`): Contains validation constraints and data shapes

### Key Conversion Principles

1. **Dual Graph Approach**: RDF vocabulary and SHACL shapes are kept in separate graphs
2. **W3C Standards Compliance**: Uses proper `rdfs:domain`, `rdfs:range`, and SHACL vocabulary
3. **Semantic Preservation**: Descriptions and constraints are preserved as semantic annotations
4. **Namespace Management**: Automatic namespace generation from filenames (e.g., `TS28623_ComDefs.yaml` → `http://ericsson.com/models/3gpp/TS28623/ComDefs#`)

## Basic Data Types

**RDF Approach**: Basic data types are converted to `rdfs:Class` entities in the RDF vocabulary, while their constraints (format, pattern, etc.) are expressed as SHACL validation rules. This separation allows the RDF to focus on semantic meaning while SHACL handles validation logic.

### String Types

#### Simple String
**OpenAPI:**
```yaml
Dn:
  description: This datatype is used for writable attribute
  type: string
```

**RDF Output:**
```turtle
@prefix ts28623_comdefs: <http://ericsson.com/models/3gpp/TS28623/ComDefs#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ts28623_comdefs:Dn a rdfs:Class ;
    rdfs:comment "This datatype is used for writable attribute" .
```

**SHACL Output:**
```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Dn .
```

#### String with Format
**OpenAPI:**
```yaml
DateTime:
  description: This datatype is used for writable attribute
  type: string
  format: date-time
```

**RDF Output:**
```turtle
ts28623_comdefs:DateTime a rdfs:Class ;
    rdfs:comment "This datatype is used for writable attribute" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:DateTime .
```

### Numeric Types

#### Float Type
**OpenAPI:**
```yaml
Float:
  description: This datatype is used for writable attribute
  type: number
  format: float
```

**RDF Output:**
```turtle
ts28623_comdefs:Float a rdfs:Class ;
    rdfs:comment "This datatype is used for writable attribute" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Float .
```

#### Integer Type
**OpenAPI:**
```yaml
IntegerType:
  type: integer
  description: A simple integer
```

**RDF Output:**
```turtle
ts28623_comdefs:IntegerType a rdfs:Class ;
    rdfs:comment "A simple integer" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:IntegerType .
```

## Object Schemas

**RDF Approach**: Object schemas become `rdfs:Class` entities with their properties converted to `rdf:Property` instances. Each property gets proper `rdfs:domain` (the class) and `rdfs:range` (the property type) relationships. SHACL shapes then add validation constraints like cardinality and data type validation.

### Simple Object with Properties

**OpenAPI:**
```yaml
DayInYear:
  type: object
  properties:
    month:
      $ref: '#/components/schemas/DateMonth'
    monthDay:
      $ref: '#/components/schemas/DateMonthDay'
```

**RDF Output:**
```turtle
ts28623_comdefs:DayInYear a rdfs:Class .

ts28623_comdefs:month a rdf:Property ;
    rdfs:domain ts28623_comdefs:DayInYear ;
    rdfs:range ts28623_comdefs:DateMonth .

ts28623_comdefs:monthDay a rdf:Property ;
    rdfs:domain ts28623_comdefs:DayInYear ;
    rdfs:range ts28623_comdefs:DateMonthDay .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:DayInYear ;
    sh:property [
        sh:path ts28623_comdefs:month ;
        sh:class ts28623_comdefs:DateMonth
    ] ;
    sh:property [
        sh:path ts28623_comdefs:monthDay ;
        sh:class ts28623_comdefs:DateMonthDay
    ] .
```

### Object with Required Properties

**OpenAPI:**
```yaml
UserProfile:
  type: object
  required:
    - name
    - email
  properties:
    name:
      type: string
      description: User's full name
    email:
      type: string
      format: email
      description: User's email address
    age:
      type: integer
      minimum: 0
      maximum: 150
      description: User's age
```

**RDF Output:**
```turtle
ts28623_comdefs:UserProfile a rdfs:Class .

ts28623_comdefs:name a rdf:Property ;
    rdfs:domain ts28623_comdefs:UserProfile ;
    rdfs:range xsd:string ;
    rdfs:comment "User's full name" .

ts28623_comdefs:email a rdf:Property ;
    rdfs:domain ts28623_comdefs:UserProfile ;
    rdfs:range xsd:string ;
    rdfs:comment "User's email address" .

ts28623_comdefs:age a rdf:Property ;
    rdfs:domain ts28623_comdefs:UserProfile ;
    rdfs:range xsd:integer ;
    rdfs:comment "User's age" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:UserProfile ;
    sh:property [
        sh:path ts28623_comdefs:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        rdfs:comment "User's full name"
    ] ;
    sh:property [
        sh:path ts28623_comdefs:email ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        rdfs:comment "User's email address"
    ] ;
    sh:property [
        sh:path ts28623_comdefs:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
        rdfs:comment "User's age"
    ] .
```

## Array Schemas

**RDF Approach**: Arrays are modeled as `rdfs:Class` entities that use SHACL's `dash:ListShape` for validation. The array items are validated using a complex SHACL path `( [ sh:zeroOrMorePath rdf:rest ] rdf:first )` that follows RDF list semantics. This approach leverages RDF's native list representation while providing proper validation constraints.

### Simple Array

**OpenAPI:**
```yaml
DnList:
  description: This datatype is used for writable attribute    
  type: array
  items:
    $ref: '#/components/schemas/Dn'
```

**RDF Output:**
```turtle
ts28623_comdefs:DnList a rdfs:Class ;
    rdfs:comment "This datatype is used for writable attribute" .
```

**SHACL Output:**
```turtle
@prefix dash: <http://datashapes.org/dash#> .

[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:DnList ;
    sh:node dash:ListShape ;
    sh:property [
        sh:path ( [ sh:zeroOrMorePath rdf:rest ] rdf:first ) ;
        sh:class ts28623_comdefs:Dn
    ] .
```

### Array with Constraints

**OpenAPI:**
```yaml
TagList:
  type: array
  items:
    type: string
    minLength: 1
    maxLength: 50
  minItems: 1
  maxItems: 10
  description: List of tags with constraints
```

**RDF Output:**
```turtle
ts28623_comdefs:TagList a rdfs:Class ;
    rdfs:comment "List of tags with constraints" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:TagList ;
    sh:node dash:ListShape ;
    sh:property [
        sh:path ( [ sh:zeroOrMorePath rdf:rest ] rdf:first ) ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
        sh:maxLength 50 ;
        sh:minCount 1 ;
        sh:maxCount 10
    ] .
```

## Reference Schemas

**RDF Approach**: References (`$ref`) are resolved to their target URIs and used directly in `rdfs:range` relationships. Internal references use the same namespace, while external references get their own namespaces. This creates a web of interconnected classes and properties that mirrors the OpenAPI reference structure.

### Internal References

**OpenAPI:**
```yaml
Location:
  type: object
  properties:
    latitude:
      $ref: '#/components/schemas/Latitude'
    longitude:
      $ref: '#/components/schemas/Longitude'
```

**RDF Output:**
```turtle
ts28623_comdefs:Location a rdfs:Class .

ts28623_comdefs:latitude a rdf:Property ;
    rdfs:domain ts28623_comdefs:Location ;
    rdfs:range ts28623_comdefs:Latitude .

ts28623_comdefs:longitude a rdf:Property ;
    rdfs:domain ts28623_comdefs:Location ;
    rdfs:range ts28623_comdefs:Longitude .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Location ;
    sh:property [
        sh:path ts28623_comdefs:latitude ;
        sh:class ts28623_comdefs:Latitude
    ] ;
    sh:property [
        sh:path ts28623_comdefs:longitude ;
        sh:class ts28623_comdefs:Longitude
    ] .
```

### External References

**OpenAPI:**
```yaml
ExternalRef:
  type: object
  properties:
    commonData:
      $ref: 'TS29571_CommonData.yaml#/components/schemas/CommonData'
```

**RDF Output:**
```turtle
@prefix ts29571_commondata: <http://ericsson.com/models/3gpp/TS29571/CommonData#> .

ts28623_comdefs:ExternalRef a rdfs:Class .

ts28623_comdefs:commonData a rdf:Property ;
    rdfs:domain ts28623_comdefs:ExternalRef ;
    rdfs:range ts29571_commondata:CommonData .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:ExternalRef ;
    sh:property [
        sh:path ts28623_comdefs:commonData ;
        sh:class ts29571_commondata:CommonData
    ] .
```

## Logical Operators

**RDF Approach**: Logical operators (`anyOf`, `oneOf`, `allOf`) are challenging in RDF because they represent complex constraints. The converter creates `rdfs:Class` entities with semantic comments explaining the logical structure, while SHACL uses `sh:or`, `sh:xone`, and `sh:and` to express the validation rules. Mixed type unions are handled by creating separate constraints for datatypes and classes.

### anyOf (Union Types)

**OpenAPI:**
```yaml
FlexibleValue:
  anyOf:
    - type: string
    - type: integer
    - $ref: '#/components/schemas/ComplexObject'
```

**RDF Output:**
```turtle
ts28623_comdefs:FlexibleValue a rdfs:Class ;
    rdfs:comment "Note: Uses OpenAPI anyOf - complex logical constraints partially supported in SHACL" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:FlexibleValue ;
    sh:or (
        [
            sh:datatype xsd:string
        ]
        [
            sh:datatype xsd:integer
        ]
        [
            sh:class ts28623_comdefs:ComplexObject
        ]
    ) .
```

### oneOf (Exclusive Union)

**OpenAPI:**
```yaml
ExclusiveChoice:
  oneOf:
    - type: string
      pattern: '^[A-Z]{2}$'
    - type: integer
      minimum: 1
      maximum: 99
```

**RDF Output:**
```turtle
ts28623_comdefs:ExclusiveChoice a rdfs:Class ;
    rdfs:comment "Note: Uses OpenAPI oneOf - complex logical constraints partially supported in SHACL" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:ExclusiveChoice ;
    sh:xone (
        [
            sh:datatype xsd:string ;
            sh:pattern "^[A-Z]{2}$"
        ]
        [
            sh:datatype xsd:integer ;
            sh:minInclusive 1 ;
            sh:maxInclusive 99
        ]
    ) .
```

### allOf (Intersection)

**OpenAPI:**
```yaml
ExtendedUser:
  allOf:
    - $ref: '#/components/schemas/UserProfile'
    - type: object
      properties:
        preferences:
          type: object
          properties:
            theme:
              type: string
              enum: [light, dark]
```

**RDF Output:**
```turtle
ts28623_comdefs:ExtendedUser a rdfs:Class ;
    rdfs:comment "Note: Uses OpenAPI allOf - complex logical constraints partially supported in SHACL" .

ts28623_comdefs:preferences a rdf:Property ;
    rdfs:domain ts28623_comdefs:ExtendedUser .

ts28623_comdefs:theme a rdf:Property ;
    rdfs:domain ts28623_comdefs:preferences ;
    rdfs:range xsd:string .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:ExtendedUser ;
    sh:property [
        sh:path ts28623_comdefs:preferences ;
        sh:node [
            sh:property [
                sh:path ts28623_comdefs:theme ;
                sh:datatype xsd:string ;
                sh:in ( "light" "dark" )
            ]
        ]
    ] .
```

## String Constraints

**RDF Approach**: String constraints are primarily handled in SHACL validation rather than RDF vocabulary. The RDF focuses on the semantic class definition, while SHACL adds `sh:pattern`, `sh:minLength`, `sh:maxLength`, and `sh:in` constraints. Format specifications like `date-time` are mapped to appropriate XSD datatypes.

### Pattern Matching

**OpenAPI:**
```yaml
Mcc:
  description: This datatype is used for writable attribute
  type: string
  pattern: '^[0-9]{3}$'
```

**RDF Output:**
```turtle
ts28623_comdefs:Mcc a rdfs:Class ;
    rdfs:comment "This datatype is used for writable attribute" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Mcc ;
    sh:datatype xsd:string ;
    sh:pattern "^[0-9]{3}$" .
```

### Length Constraints

**OpenAPI:**
```yaml
ShortString:
  type: string
  minLength: 5
  maxLength: 20
  description: String with length constraints
```

**RDF Output:**
```turtle
ts28623_comdefs:ShortString a rdfs:Class ;
    rdfs:comment "String with length constraints" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:ShortString ;
    sh:datatype xsd:string ;
    sh:minLength 5 ;
    sh:maxLength 20 .
```

### Enumeration

**OpenAPI:**
```yaml
Status:
  type: string
  enum:
    - active
    - inactive
    - pending
  description: Status enumeration
```

**RDF Output:**
```turtle
ts28623_comdefs:Status a rdfs:Class ;
    rdfs:comment "Status enumeration" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Status ;
    sh:datatype xsd:string ;
    sh:in ( "active" "inactive" "pending" ) .
```

## Numeric Constraints

**RDF Approach**: Numeric types are converted to `rdfs:Class` entities with appropriate XSD datatypes (`xsd:integer`, `xsd:float`, `xsd:double`). Range constraints (`minimum`, `maximum`) are expressed as SHACL `sh:minInclusive` and `sh:maxInclusive` properties, maintaining the validation semantics while keeping the RDF vocabulary clean.

### Range Constraints

**OpenAPI:**
```yaml
Latitude:
  type: number
  format: float
  minimum: -90
  maximum: 90
```

**RDF Output:**
```turtle
ts28623_comdefs:Latitude a rdfs:Class .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Latitude ;
    sh:datatype xsd:float ;
    sh:minInclusive -90 ;
    sh:maxInclusive 90 .
```

### Integer Constraints

**OpenAPI:**
```yaml
Age:
  type: integer
  minimum: 0
  maximum: 150
  description: Age in years
```

**RDF Output:**
```turtle
ts28623_comdefs:Age a rdfs:Class ;
    rdfs:comment "Age in years" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Age ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150 .
```

## OpenAPI-Specific Features

**RDF Approach**: OpenAPI-specific features like `readOnly`, `writeOnly`, `nullable`, and `deprecated` don't have direct RDF equivalents. The converter preserves these as semantic comments using `rdfs:comment`, allowing implementers to understand the original OpenAPI semantics while maintaining RDF's focus on structural relationships.

### Read-Only Properties

**OpenAPI:**
```yaml
FloatRo:
  description: This datatype is used for readOnly attribute
  type: number
  format: float
  readOnly: true
```

**RDF Output:**
```turtle
ts28623_comdefs:FloatRo a rdfs:Class ;
    rdfs:comment "This datatype is used for readOnly attribute" ;
    rdfs:comment "Note: This property is readOnly in OpenAPI - consider access control in implementation" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:FloatRo ;
    sh:datatype xsd:float .
```

### Write-Only Properties

**OpenAPI:**
```yaml
Password:
  type: string
  writeOnly: true
  description: User password
```

**RDF Output:**
```turtle
ts28623_comdefs:Password a rdfs:Class ;
    rdfs:comment "User password" ;
    rdfs:comment "Note: This property is writeOnly in OpenAPI - consider access control in implementation" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:Password ;
    sh:datatype xsd:string .
```

### Nullable Properties

**OpenAPI:**
```yaml
OptionalString:
  type: string
  nullable: true
  description: Optional string that can be null
```

**RDF Output:**
```turtle
ts28623_comdefs:OptionalString a rdfs:Class ;
    rdfs:comment "Optional string that can be null" ;
    rdfs:comment "Note: This property is nullable in OpenAPI - null vs absent semantics not preserved in RDF" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:OptionalString ;
    sh:datatype xsd:string .
```

### Deprecated Schemas

**OpenAPI:**
```yaml
OldSchema:
  type: string
  deprecated: true
  description: This schema is deprecated
```

**RDF Output:**
```turtle
ts28623_comdefs:OldSchema a rdfs:Class ;
    rdfs:comment "This schema is deprecated" ;
    rdfs:comment "Note: This schema is deprecated in OpenAPI" .
```

**SHACL Output:**
```turtle
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:OldSchema ;
    sh:datatype xsd:string .
```

## Namespace Management

**RDF Approach**: Namespaces are crucial in RDF for avoiding URI conflicts and creating modular vocabularies. The converter automatically generates namespaces from filenames using a configurable prefix, creating a consistent URI structure that mirrors the OpenAPI file organization. External references get their own namespaces, enabling proper cross-file relationships.

### Automatic Namespace Generation

The converter automatically generates namespaces based on filenames:

- **Pattern**: `TS28xxx_Name.yaml` → `http://ericsson.com/models/3gpp/TS28xxx/Name#`
- **Example**: `TS28623_ComDefs.yaml` → `http://ericsson.com/models/3gpp/TS28623/ComDefs#`

### Custom Namespace Prefix

You can specify a custom namespace prefix:

```bash
openapi-to-rdf convert file.yaml --namespace-prefix "https://myorg.com/models/"
```

This generates: `https://myorg.com/models/TS28623/ComDefs#`

### External Reference Handling

External references are automatically resolved and namespaced:

```yaml
# In TS28623_ComDefs.yaml
externalRef:
  $ref: 'TS29571_CommonData.yaml#/components/schemas/CommonData'
```

**Generated Namespace:**
```turtle
@prefix ts29571_commondata: <http://ericsson.com/models/3gpp/TS29571/CommonData#> .
```

## Output Structure

**RDF Approach**: The dual-graph approach separates concerns: RDF vocabulary focuses on semantic relationships (classes, properties, domains, ranges) while SHACL shapes handle validation logic (constraints, cardinalities, data types). This separation allows RDF consumers to understand the data model without being overwhelmed by validation details, and SHACL validators to focus purely on constraint checking.

### File Organization

The converter generates two files for each OpenAPI specification:

1. **`{filename}_rdf.ttl`** - RDF vocabulary
2. **`{filename}_shacl.ttl`** - SHACL validation shapes

### RDF Vocabulary Structure

```turtle
@prefix ts28623_comdefs: <http://ericsson.com/models/3gpp/TS28623/ComDefs#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
ts28623_comdefs:ClassName a rdfs:Class ;
    rdfs:comment "Description from OpenAPI" .

# Properties
ts28623_comdefs:propertyName a rdf:Property ;
    rdfs:domain ts28623_comdefs:ClassName ;
    rdfs:range xsd:string ;
    rdfs:comment "Property description" .
```

### SHACL Shapes Structure

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix dash: <http://datashapes.org/dash#> .
@prefix ts28623_comdefs: <http://ericsson.com/models/3gpp/TS28623/ComDefs#> .

# Node Shapes
[] a sh:NodeShape ;
    sh:targetClass ts28623_comdefs:ClassName ;
    sh:property [
        sh:path ts28623_comdefs:propertyName ;
        sh:datatype xsd:string ;
        sh:minCount 1
    ] .
```

## Conversion Limitations and Notes

### Partially Supported Features

1. **Logical Operators**: `anyOf`, `oneOf`, `allOf` are converted but complex nested logical constraints may not be fully expressible in SHACL
2. **Discriminators**: OpenAPI discriminators are noted but not fully converted to OWL union classes
3. **Custom Formats**: Only standard XSD formats are mapped; custom formats are noted in comments

### Semantic Comments

The converter adds semantic comments for OpenAPI features that don't translate directly to RDF:

- `readOnly`/`writeOnly` properties
- `nullable` properties  
- `deprecated` schemas
- Custom format constraints
- Logical operator usage

### Best Practices

1. **Validation**: Use SHACL shapes for data validation
2. **Vocabulary**: Use RDF vocabulary for semantic modeling
3. **Namespaces**: Leverage automatic namespace generation for consistency
4. **External References**: Include external YAML files in the `external_refs` parameter

This documentation provides comprehensive coverage of all conversion patterns supported by the OpenAPI to RDF/SHACL converter, enabling users to understand exactly how their OpenAPI specifications will be transformed into semantic web standards.
