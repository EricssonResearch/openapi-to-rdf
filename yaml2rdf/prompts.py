from langchain_core.prompts import PromptTemplate

generate_rdf_prefixes_prompt = PromptTemplate(
    template="""
Generate the proper namespaces for a set of external references. The namespace should be the filename without yaml extension. So that cross-reference between multiple RDF produced is consistent. Include also prefix for RDF, RDFS, OWL, XMLSchema.

DO NOT explanain yourself, the output must be ready to be part of a .ttl file, i.e. NO markdown escape quotes. 

# Example

    ## Input
    Filename: 
            .../TS28541_SliceNrm.yaml
    Namespace: 
        https://forge.3gpp.org/rep/sa5/MnS/-/tree/Rel-19/OpenAPI/# 
    References: 
        [ 'TS28541_NrNrm.yaml', 'TS28623_ComDefs.yaml']


    # Output
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
    @prefix TS28541_SliceNrm: <https://forge.3gpp.org/rep/sa5/MnS/-/tree/Rel-19/OpenAPI/TS28541_SliceNrm.yaml#> .
    @prefix TS28541_NrNrm: <https://forge.3gpp.org/rep/sa5/MnS/-/tree/Rel-19/OpenAPI/TS28541_NrNrm.yaml#> .
    @prefix TS28623_ComDefs: <https://forge.3gpp.org/rep/sa5/MnS/-/tree/Rel-19/OpenAPI/TS28623_ComDefs.yaml#> .

# INPUT
Filename:
    {yaml_file}
Namespace: 
    {namespace}
References: 
    {yaml_references}
"""
)

generate_rdf_content_prompt = PromptTemplate(
    template="""
 Prompt Title: Convert OpenAPI YAML to OWL/Turtle

Instructions
 1. Naming Conventions
    - Replace all dashes/hyphens in class or property names with underscores (`my-property` → `my_property`).
    - Do not generate prefixes. Use the filename (minus extension) as the prefix.  
    - Example: `TS28541_SliceNrm.yaml` → `TS28541_SliceNrm:`  

 2. Classes vs. Properties
    - If `type: object`, define an `owl:Class`.  
    - If `type: string` (and no `enum`), define an `owl:DatatypeProperty` (range `xsd:string`).  
    - If `enum` is present, define an `owl:Class` with `owl:oneOf` enumerated individuals.  
    - If `$ref` is present, define an `owl:ObjectProperty` if the referenced schema is an object class. If the referenced schema is known to be a numeric/string “type,” define an `owl:DatatypeProperty`.  

 3. Cardinality
    - If a property is in `required`, use `owl:minCardinality 1`.  
    - If only one value can be assigned, also define it as `owl:FunctionalProperty` or set `owl:maxCardinality 1` as appropriate.  
    
 4. External References
    - Do not redefine external classes or properties. Use their known prefixes if they exist.  
    - If the external reference includes a dash, convert it to underscore for the local name.  

 5. Arrays
    - For arrays of simple types, define an `owl:DatatypeProperty` or repeated property approach.  
    - For arrays referencing an object, define an `owl:ObjectProperty` with potentially multiple cardinalities, e.g., `owl:maxCardinality` not set or an explicit number if it’s known.  

 6. Output Format
    - Produce valid Turtle with no markdown or explanation.  
    - End each triple with a period.  
    - Make sure to parse in any standard RDF/Turtle validator without errors.  

 7. Examples

# Example 1

    ## input (YAML)
        Filename: 
            .../TS28541_SliceNrm.yaml
        Content: 
            XLThpt:
            type: object
            properties:
                servAttrCom:
                    $ref: '#/components/schemas/ServAttrCom'
                guaThpt:
                    $ref: 'TS28623_ComDefs.yaml#/components/schemas/FloatRo'
                maxThpt:
                    $ref: 'TS28623_ComDefs.yaml#/components/schemas/FloatRo'
    
    # output (RDF-Owl)
        #### Define the class for XLThpt
        TS28541_SliceNrm:XLThpt rdf:type owl:Class ;
                                rdfs:label "XLThpt" ;
                                rdfs:comment "The XLThpt class represents an object with guaranteed and maximum throughput." .

        #### Define the property servAttrCom with a cardinality constraint (1 occurrence)
        TS28541_SliceNrm:servAttrCom rdf:type owl:ObjectProperty ;
                                    rdfs:domain TS28541_SliceNrm:XLThpt ;
                                    rdfs:range TS28541_SliceNrm:ServAttrCom ;
                                    rdfs:label "servAttrCom" ;
                                    rdfs:comment "This property refers to a ServAttrCom object." ;
                                    owl:minCardinality 1 .

        #### Define the property guaThpt with a cardinality constraint (1 occurrence)
        TS28541_SliceNrm:guaThpt rdf:type owl:DatatypeProperty ;
                                rdfs:domain TS28541_SliceNrm:XLThpt ;
                                rdfs:range TS28623_ComDefs:FloatRo ;
                                rdfs:label "guaThpt" ;
                                rdfs:comment "Guaranteed throughput." ;
                                owl:minCardinality 1 .

        #### Define the property maxThpt with a cardinality constraint (1 occurrence)
        TS28541_SliceNrm:maxThpt rdf:type owl:DatatypeProperty ;
                                rdfs:domain TS28541_SliceNrm:XLThpt ;
                                rdfs:range TS28623_ComDefs:FloatRo ;
                                rdfs:label "maxThpt" ;
                                rdfs:comment "Maximum throughput." ;
                                owl:minCardinality 1 .

# Example 2

    ## input (YAML)
        Filename: 
            .../TS28541_SliceNrm.yaml
        Content: 
            MobilityLevel:
            type: string
            enum:
                - STATIONARY
                - NOMADIC
                - RESTRICTED_MOBILITY
                - FULL_MOBILITY

    ## output (RDF)
        #### Define the MobilityLevel class as an enumeration
        TS28541_SliceNrm:MobilityLevel rdf:type owl:Class ;
                                    rdfs:label "Mobility Level" ;
                                    rdfs:comment "Enumeration of mobility levels." ;
                                    owl:oneOf (TS28541_SliceNrm:STATIONARY
                                                TS28541_SliceNrm:NOMADIC
                                                TS28541_SliceNrm:RESTRICTED_MOBILITY
                                                TS28541_SliceNrm:FULL_MOBILITY) .

        #### Define the enumerated values
        TS28541_SliceNrm:STATIONARY rdf:type owl:NamedIndividual ;
                                    rdfs:label "STATIONARY" ;
                                    rdfs:comment "Indicates that the mobility level is stationary." .

        TS28541_SliceNrm:NOMADIC rdf:type owl:NamedIndividual ;
                                rdfs:label "NOMADIC" ;
                                rdfs:comment "Indicates that the mobility level is nomadic." .

        TS28541_SliceNrm:RESTRICTED_MOBILITY rdf:type owl:NamedIndividual ;
                                            rdfs:label "RESTRICTED_MOBILITY" ;
                                            rdfs:comment "Indicates that the mobility level is restricted mobility." .

        TS28541_SliceNrm:FULL_MOBILITY rdf:type owl:NamedIndividual ;
                                    rdfs:label "FULL_MOBILITY" ;
                                    rdfs:comment "Indicates that the mobility level is full mobility." .

        #### Define the mobilityLevel property that can only take these enumerated values
        TS28541_SliceNrm:mobilityLevel rdf:type owl:ObjectProperty ;
                                    rdfs:domain TS28541_SliceNrm:SliceNrm ;
                                    rdfs:range TS28541_SliceNrm:MobilityLevel ;
                                    rdfs:label "mobilityLevel" ;
                                    rdfs:comment "Property representing the mobility level." .


# Example 3

    ## input (YAML)
        Filename: 
            .../TS28541_SliceNrm.yaml
        Content:
            openapi: 3.0.1
            info:
            title: Slice NRM
            version: 19.0.0
            description: >-
                OAS 3.0.1 specification of the Slice NRM
                @ 2024, 3GPP Organizational Partners (ARIB, ATIS, CCSA, ETSI, TSDSI, TTA, TTC).
                All rights reserved.
            externalDocs:
            description: 3GPP TS 28.541; 5G NRM, Slice NRM
            url: http://www.3gpp.org/ftp/Specs/archive/28_series/28.541/
    
    ## output (RDF)
        #### Define the SliceNRM class
        TS28541_SliceNrm:SliceNRM rdf:type owl:Class ;
                                rdfs:label "Slice NRM" ;
                                rdfs:comment "Class representing the Slice NRM document metadata." .

        #### Define the version property as functional (only one value allowed)
        TS28541_SliceNrm:version rdf:type owl:FunctionalProperty ;
                                rdfs:domain TS28541_SliceNrm:SliceNRM ;
                                rdfs:range xsd:string ;
                                rdfs:label "Version" ;
                                rdfs:comment "The version of the document." .

        #### Define the description property
        TS28541_SliceNrm:description rdf:type owl:DatatypeProperty ;
                                    rdfs:domain TS28541_SliceNrm:SliceNRM ;
                                    rdfs:range xsd:string ;
                                    rdfs:label "Description" ;
                                    rdfs:comment "A description of the document." .

        #### Define the URL property
        TS28541_SliceNrm:url rdf:type owl:DatatypeProperty ;
                            rdfs:domain TS28541_SliceNrm:SliceNRM ;
                            rdfs:range xsd:string ;
                            rdfs:label "URL" ;
                            rdfs:comment "The URL for external documentation related to the document." .

        #### Example instance of SliceNRM
        TS28541_SliceNrm:SliceNRMInstance rdf:type TS28541_SliceNrm:SliceNRM ;
                                        TS28541_SliceNrm:version "19.0.0" ;
                                        TS28541_SliceNrm:description "OAS 3.0.1 specification of the Slice NRM @ 2024, 3GPP Organizational Partners (ARIB, ATIS, CCSA, ETSI, TSDSI, TTA, TTC). All rights reserved." ;
                                        TS28541_SliceNrm:url "http://www.3gpp.org/ftp/Specs/archive/28_series/28.541/" .

# Example 4
    ## input (YAML)
        Filename: 
            .../TS28541_SliceNrm.yaml
        Content:
            resources-sliceNrm:
                oneOf:             
                - $ref: '#/components/schemas/NetworkSlice-Single'
                - $ref: '#/components/schemas/NetworkSliceSubnet-Single'
                - $ref: '#/components/schemas/EP_Transport-Single'
                - $ref: '#/components/schemas/NetworkSliceSubnetProviderCapabilities-Single'
                - $ref: '#/components/schemas/FeasibilityCheckAndReservationJob-Single'
                - $ref: '#/components/schemas/NetworkSliceController-Single'
                - $ref: '#/components/schemas/NetworkSliceSubnetController-Single'
                - $ref: '#/components/schemas/IsolationProfile-Single'
    
    ## output (RDF-Owl)
        #### Define the resources_sliceNrm class
        TS28541_SliceNrm:Resources_SliceNrm rdf:type owl:Class ;
            rdfs:label "Resources Slice NRM" ;
            rdfs:comment "Represents a resource that can be one of several types related to slice NRM." .

        #### Define an owl:oneOf relationship for resources_sliceNrm
        TS28541_SliceNrm:Resources_SliceNrm owl:oneOf (
            TS28541_SliceNrm:NetworkSlice_Single
            TS28541_SliceNrm:NetworkSliceSubnet_Single
            TS28541_SliceNrm:EP_Transport_Single
            TS28541_SliceNrm:NetworkSliceSubnetProviderCapabilities_Single
            TS28541_SliceNrm:FeasibilityCheckAndReservationJob_Single
            TS28541_SliceNrm:NetworkSliceController_Single
            TS28541_SliceNrm:NetworkSliceSubnetController_Single
            TS28541_SliceNrm:IsolationProfile_Single
        ) .


*Given the following YAML input, convert it to OWL/Turtle according to the rules above…*
Filename: 
    {filename}
Content: 
    {content}
"""
)

generate_rdf_header_prompt = PromptTemplate(
    template="""
Given an OpenAPI file (YAML, JSON) and its header, you convert it to RDF ttl using Owl.
Always use undescores instead of dashs for classes or properties names. 

DO NOT explanain yourself, the output must be ready to be part of a .ttl file, i.e. NO markdown escape quotes. 

Use the following example as reference 

# Example 1
    ## input (YAML)
        Filename: 
            .../TS28541_SliceNrm.yaml
        Content:
            openapi: 3.0.1
            info:
            title: Slice NRM
            version: 19.0.0
            description: >-
                OAS 3.0.1 specification of the Slice NRM
                @ 2024, 3GPP Organizational Partners (ARIB, ATIS, CCSA, ETSI, TSDSI, TTA, TTC).
                All rights reserved.
            externalDocs:
            description: 3GPP TS 28.541; 5G NRM, Slice NRM
            url: http://www.3gpp.org/ftp/Specs/archive/28_series/28.541/
    
    ## output (RDF)
        #### Define the SliceNRM class
        TS28541_SliceNrm:SliceNRM rdf:type owl:Class ;
                                rdfs:label "Slice NRM" ;
                                rdfs:comment "Class representing the Slice NRM document metadata." .

        #### Define the version property as functional (only one value allowed)
        TS28541_SliceNrm:version rdf:type owl:FunctionalProperty ;
                                rdfs:domain TS28541_SliceNrm:SliceNRM ;
                                rdfs:range xsd:string ;
                                rdfs:label "Version" ;
                                rdfs:comment "The version of the document." .

        #### Define the description property
        TS28541_SliceNrm:description rdf:type owl:DatatypeProperty ;
                                    rdfs:domain TS28541_SliceNrm:SliceNRM ;
                                    rdfs:range xsd:string ;
                                    rdfs:label "Description" ;
                                    rdfs:comment "A description of the document." .

        #### Define the URL property
        TS28541_SliceNrm:url rdf:type owl:DatatypeProperty ;
                            rdfs:domain TS28541_SliceNrm:SliceNRM ;
                            rdfs:range xsd:string ;
                            rdfs:label "URL" ;
                            rdfs:comment "The URL for external documentation related to the document." .

        #### Example instance of SliceNRM
        TS28541_SliceNrm:SliceNRMInstance rdf:type TS28541_SliceNrm:SliceNRM ;
                                        TS28541_SliceNrm:version "19.0.0" ;
                                        TS28541_SliceNrm:description "OAS 3.0.1 specification of the Slice NRM @ 2024, 3GPP Organizational Partners (ARIB, ATIS, CCSA, ETSI, TSDSI, TTA, TTC). All rights reserved." ;
                                        TS28541_SliceNrm:url "http://www.3gpp.org/ftp/Specs/archive/28_series/28.541/" .

# Convert the following input content.
Filename: 
    {filename}
Content: 
    {content}
"""
)
