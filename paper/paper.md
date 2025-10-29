---
title: "OpenAPI to RDF: Automated Generation of RDF Vocabularies and SHACL Validation Shapes from OpenAPI Schema"
tags:
  - Python
  - OpenAPI
  - RDF
  - SHACL
  - semantic web
  - data validation
  - telecommunications
authors:
  - name: Jean Martins
    orcid: 0000-0003-2112-3723
    affiliation: 1
  - name: Leonid Mokrushin 
    orcid: 0009-0007-3438-2720
    affiliation: 1
  - name: Marin Orlic
    orcid: 0009-0008-8411-5935
    affiliation: 1
affiliations:
 - name: Ericsson Research
   index: 1
date: 28 October 2025
bibliography: paper.bib
---

# Summary

`openapi-to-rdf` is a Python package that automatically converts data schemas from OpenAPI specifications into semantic web formats. OpenAPI is a widely-used standard for describing web APIs and data structures, commonly used in telecommunications, cloud computing, and web services. While OpenAPI specifications include API endpoints, parameters, and responses, this tool specifically focuses on the schema definitions that describe the structure and constraints of data models.

The package transforms these schema definitions from OpenAPI YAML files into two complementary outputs: RDF (Resource Description Framework) vocabularies that define concepts and their relationships, and SHACL (Shapes Constraint Language) validation shapes that enable automated data validation. This conversion bridges the gap between syntactic schema definitions and semantic web technologies, enabling automated reasoning, validation, and integration across different systems.

The converter handles all major OpenAPI schema features including nested objects, arrays, enumerations, and complex logical relationships (`oneOf`, `anyOf`, `allOf`). It resolves cross-references between multiple specification files and generates globally unique identifiers for all schema components. The package provides both command-line tools and a Python API, with comprehensive quality assurance reporting.

# Statement of Need

Modern software systems increasingly rely on APIs (Application Programming Interfaces) to communicate and share data. OpenAPI has emerged as the standard for documenting these APIs, with thousands of specifications available across industries. While OpenAPI specifications describe complete APIs including endpoints and operations, the schema definitions within these specifications -- which define data structures, validation rules, and relationships -- exist in isolation without explicit semantic relationships to other schemas, making integration and automated processing difficult.

The telecommunications industry exemplifies this challenge. The 3rd Generation Partnership Project (3GPP) publishes hundreds of OpenAPI specifications defining network functions and data models [@3GPP2023]. These specifications contain rich schema definitions describing network elements, policies, and service data structures. Meanwhile, industry frameworks like TM Forum's Intent-Based Automation require semantic representations using RDF and SHACL for automated network management [@TMForumTR290v2024; @TMForumTR2922024]. Currently, bridging between these syntactic schema definitions and semantic frameworks requires manual, error-prone translation.

Existing tools for OpenAPI processing focus on code generation, documentation, or testing, but none systematically convert schema definitions to semantic web formats. Academic research has explored individual aspects of API-to-RDF conversion but lacks comprehensive tooling for real-world schema specifications with complex dependencies and domain-specific requirements.

The `openapi-to-rdf` package addresses this gap by providing automated conversion from OpenAPI schema definitions to W3C-standard semantic formats [@W3C2014RDF; @W3C2017SHACL]. This enables:

- **Intent-based automation**: Enable semantic representation of telecom network models for TM Forum Intent-Based Automation frameworks
- **Domain knowledge integration**: Convert 3GPP information models from OpenAPI schemas to RDF vocabularies to extend TM Forum Intent Ontology (TIO)
- **Automated SHACL validation**: Generate SHACL validation shapes that enable automated verification of intent compliance against domain specific 3GPP models

Therefore, the tool has particular relevance for telecommunications, where it enables intent-based network management systems, but applies broadly to any domain using OpenAPI specifications including cloud platforms, IoT systems, and web services.


# References