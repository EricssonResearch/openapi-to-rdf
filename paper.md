---
title: "OpenAPI to RDF Converter: Automated Generation of RDF Vocabularies and SHACL Validation Shapes from OpenAPI Specifications"
tags:
  - Python
  - OpenAPI
  - RDF
  - SHACL
  - semantic web
  - data validation
  - schema conversion
  - W3C standards
  - 3GPP telecommunications
  - TM Forum
authors:
  - name: Jean Martins, Leonid Mokrushin, Marin Orlic
    orcid: 0000-0000-0000-0000
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Ericsson
   index: 1
date: 15 October 2025
bibliography: paper.bib
---

# Summary

The **OpenAPI to RDF Converter** is a Python tool that transforms **3GPP OpenAPI YAML specifications** into **RDF vocabularies** and **SHACL validation shapes** required for **TM Forum intent-based automation**. This conversion addresses a key interoperability challenge: TM Forum intents must be represented in RDF, but 3GPP specifications—the authoritative definitions of mobile network functions—are published as OpenAPI and YANG artifacts that cannot be directly used in semantic intent frameworks.

The converter bridges this gap by generating **domain-specific RDF ontologies** aligned with 3GPP models, enabling operators to author machine-understandable intents consistent with standardized network models. It simultaneously produces **SHACL validation shapes**, allowing automated conformance checking of intents against 3GPP constraints using off-the-shelf SHACL validators.

This approach creates a reproducible and evolvable path between 3GPP’s service-based architecture (SBA) APIs and TM Forum’s RDF-based intent management ecosystem—supporting formal validation, semantic interoperability, and automation at scale.

# Statement of Need

Telecommunications operators are progressing toward **autonomous networks** driven by high-level business intents rather than manual configuration. Within TM Forum's *Intent-Based Automation* framework, an *intent* is defined as "the formal specification of all expectations including requirements, goals and constraints given to a technical system" [@TMForumANLAV2024]. The framework's **Intent Common Model (TR290v)** and **Intent Ontology (TIO)** define how intents are expressed semantically using **RDF vocabularies** and **SHACL constraints** [@TMForumTR290v2024; @TMForumTR2922024].

However, TM Forum intentionally provides only **domain-agnostic** ontologies. It explicitly delegates the definition of **domain-specific extensions**—such as those describing radio, core, and transport network elements—to the implementing organizations and standards development bodies. In practice, this means that operators must map domain models from **3GPP specifications** (written in OpenAPI or YANG) into RDF before they can use them for intent representation.

3GPP's RESTful Service-Based Architecture (SBA), introduced in *TS 29.501* and expanded across many *TS 29.xxx* specifications, defines hundreds of OpenAPI schemas describing network functions, policies, and services across releases 15 to 19+ [@3GPP2023]. These are published as YAML files on 3GPP's public Forge repositories. While these APIs define the authoritative data and interface models, they are **syntactic**, not **semantic**. TM Forum's intent framework, by contrast, requires RDF [@W3C2014RDF] for semantic interoperability, graph reasoning, and formal validation.

This misalignment creates a **critical format gap**:

- **Incompatible representations:** 3GPP’s OpenAPI/YANG models cannot be used directly in RDF-based intent frameworks.  
- **Manual conversion bottleneck:** Manual transformation of OpenAPI to RDF is slow, error-prone, and cannot keep pace with frequent 3GPP release cycles.  
- **Validation requirement:** TM Forum’s **Autonomous Networks Level Assessment and Validation (ANLAV)** framework requires formal, machine-readable validation of intents against constraints.  
- **Cross-domain consistency:** Intent-driven systems integrate multiple domains (e.g., radio + core + transport), making consistent ontology generation across evolving 3GPP releases essential.  

Recent TM Forum efforts—such as **TR294A**, which defines the model connection between TM Forum intents and *3GPP TS 28.312 Intent Extension Models*—reinforce the need for reliable, machine-generated RDF versions of 3GPP data models [@TMForumTR294A2024]. Yet, no publicly available tooling automates this translation.

The **OpenAPI to RDF Converter** addresses this unmet need by automating the generation of **RDF vocabularies** and **SHACL validation shapes** directly from 3GPP OpenAPI definitions. It operationalizes TM Forum’s guidance by providing:

1. Automated and version-aware translation of OpenAPI schemas to RDF vocabularies consistent with TM Forum’s TIO;  
2. SHACL shapes that formalize 3GPP validation constraints for automated intent verification;  
3. Namespace management and cross-reference resolution to maintain semantic integrity across specifications.

This enables operators to construct **3GPP domain-specific intent models** compatible with TM Forum’s intent framework, thereby bridging two essential standards ecosystems: 3GPP’s API-driven data modeling and TM Forum’s RDF-based autonomous network management.

# Functionality and Architecture

The converter implements a **dual-output architecture**, supporting both SHACL-based (default) and OWL-based output modes. The SHACL mode aligns directly with TM Forum’s validation-oriented intent frameworks, while the OWL mode supports ontology reasoning and knowledge graph integration.

## SHACL Conversion (Default)

Following W3C best practices [@W3C2014RDF; @W3C2017SHACL], two complementary artifacts are produced:

- **RDF Vocabulary (`*_rdf.ttl`)**: Defines classes and properties, with appropriate `rdfs:domain`, `rdfs:range`, and human-readable documentation (`rdfs:comment`).  
- **SHACL Shapes (`*_shacl.ttl`)**: Defines validation constraints (`sh:NodeShape`, `sh:PropertyShape`) that mirror OpenAPI schema rules.

## Core Technical Features

**Comprehensive Schema Support:**  
Supports all OpenAPI schema constructs [@OpenAPI2021]—objects, arrays, enumerations, and compositional logic (`oneOf`, `anyOf`, `allOf`)—with faithful translation to SHACL constructs.

**Logical Operator Mapping:**  
- `oneOf` → `sh:xone` (exclusive alternatives)  
- `anyOf` → `sh:or` (constraint disjunction)  
- `allOf` → `sh:and` (constraint conjunction)  

**Namespace and Version Management:**  
Namespaces are derived from OpenAPI filenames and version metadata, ensuring globally unique URIs. The converter automatically resolves `$ref` dependencies across multiple OpenAPI files, with conflict detection and cycle prevention.

**Documentation Preservation:**  
Descriptions and notes in OpenAPI are preserved as `rdfs:comment` and `skos:definition` statements, enabling readable ontologies.

**Quality Validation Framework:**  
A self-validation stage computes coverage metrics:
- Schema completeness  
- Property completeness  
- Description preservation  
- SHACL syntax correctness  

These are automatically tested using W3C SHACL validators [@W3C2017SHACL] during the conversion process.

```python
# Example validation summary from TS28623_ComDefs.yaml
Schema Coverage: 100% (64/64)
Property Coverage: 100% (30/30)
Description Preservation: 100% (35/35)
Overall Quality Score: 100% — EXCELLENT
````

## Integration with 3GPP Specifications

The converter natively supports **3GPP Forge repositories**, implementing:

* **Automated discovery** of OpenAPI YAML files via GitLab API and pattern matching;
* **Release management** for Rel-15 → Rel-19+, adapting to evolving schema organization;
* **Cross-specification resolution** for shared definitions (e.g., *CommonData.yaml*);
* **Fallback YANG translation** for cases where only YANG models exist.

This capability enables a sustainable bridge from 3GPP’s OpenAPI-defined network models to RDF vocabularies usable within TM Forum’s intent and ANLAV ecosystems.

# Quality Assurance and Validation

The converter includes an integrated **quality assurance pipeline** that verifies:

* **Completeness:** All schemas and properties are converted.
* **Semantic correctness:** Each property has correct domain/range assignments.
* **SHACL validation:** All generated SHACL shapes pass syntax and structural checks.
* **Cross-reference integrity:** All `$ref` dependencies resolve correctly across files.
* **Namespace uniqueness:** Conflicting prefixes are detected and flagged.

Regression testing is performed on representative 3GPP specifications, including:

* **TS 28.541** (Network Slice Management)
* **TS 28.111** (Fault Management)
* **TS 28.532** (Performance Management)

Each shows full conversion with validated SHACL output. Benchmark tests demonstrate linear time complexity relative to schema count, supporting practical use across hundreds of OpenAPI definitions per release.

# Impact and Applications

The OpenAPI to RDF Converter delivers several key benefits across the telecommunications automation landscape:

**1. 3GPP Domain-Specific Intent Specification**
Enables operators to express TM Forum intents using canonical 3GPP network models in RDF, ensuring semantic consistency between operational systems and intent-based orchestration platforms.

**2. Automated Validation and ANLAV Compliance**
Generated SHACL shapes support formal validation of intents, aligning with TM Forum’s ANLAV framework and facilitating autonomous verification of cognitive abilities (Intent, Awareness, Analysis, Decision, Execution).

**3. Cross-Domain Semantic Interoperability**
Creates a foundation for integrating multiple technology domains—3GPP, ETSI, O-RAN, or vendor-specific APIs—within a unified RDF-based ecosystem.

**4. Evolution-Resilient Architecture**
Handles continuous 3GPP evolution (Rel-15 → Rel-19+) through automated schema retrieval and version management, ensuring sustainable ontology generation.

**5. Broader Industry Applicability**
Beyond telecommunications, the methodology applies to any domain using OpenAPI, providing a blueprint for RDF-based validation and semantic interoperability across industries (cloud, IoT, energy).

**6. Open Source Foundation for Collaboration**
The converter is published as open source, providing a foundation for community-driven improvement and alignment with emerging TM Forum Catalyst initiatives such as **Intent-Driven Autonomous Networks (IDAN)** [@TMForumIDAN2024].

# Future Work

Several research and engineering challenges remain:

* Enhancing translation of nested logical constraints (`not`, conditional schemas).
* Introducing **round-trip validation** between SHACL and OpenAPI to ensure semantic fidelity.
* Extending support for **temporal and cross-domain constraints**, as required in complex intent scenarios.
* Formal mapping to **TM Forum’s TR294A Intent Extension Model** for consistent ontology alignment.
* Benchmarking performance on large-scale 3GPP specification sets for industrial deployment.

These enhancements will further solidify the tool’s role as a standards-aligned bridge between syntax-based APIs and semantic intent ecosystems.

# Acknowledgements

The author acknowledges the **TM Forum** for pioneering intent-based automation and the **3GPP community** for maintaining comprehensive OpenAPI specifications essential to modern telecommunications. Special recognition is given to the **W3C RDF and SHACL communities**, whose open standards make formal semantic validation possible.

# References