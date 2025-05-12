:- module(openapi, [ openapi_read/2,
                     clear_cache/0 ]).

/*

components:
  schemas:
     MyObj:
       type: object                      :MyObj a rdfs:Class ;
       properties:
         MyProperty:                     :MyProperty a rdf:Property ;
                                         :MyObj cc:definedProperty :MyProperty ;

       allOf:
         - $ref: 'TS../schemas/Top'      :MyObj a rdfs:Class ;
         - type: object                         rdfs:subClassOf :Top ;

       allOf:                            sh:and
       anyOf:                            sh:or
       oneOf:                            sh:xone
       not:                              sh:not

                                         sh:property [ sh:path MyObj ;
             string                                    sh:datatype xsd:string ;
       pattern: '^[A-Fa-f0-9]{6}$'                     sh:pattern: "^[A-Fa-f0-9]{6}$" ;
       minLength: 0                                    sh:minLength 0 ;
       maxLength: 255                                  sh:maxLength 255 ;

       enum:                                           sh:in ( "UP" "DOWN" ) ;
         - UP
         - DOWN
       default: UP                                     sh:defaultValue "UP" ;

             number
       minimum: 0                                      sh:minInclusive 0 ;
       maximum: 100                                    sh:maxInclusive 100 ;
       format: float                                   sh:datatype xsd:float;
       format: double                                  sh:datatype xsd:double;
             integer                                   sh:datatype xsd:integer;

             date-time                                 sh:datatype xsd:dateTime;
             full-time                                 sh:datatype xsd:time;
             date-month                                sh:datatype xsd:gMonth;
             date-mday                                 sh:datatype xsd:gMonthDay;
             uuid                                      sh:datatype ?;

                                                       @prefix dash: <https://datashapes.org/dash#>.
             array                                     sh:node dash:ListShape ;
       items:                                          sh:property [
                                                         sh:path ( [ sh:zeroOrMorePath rdf:rest ] rdf:first ) ;
         type: string                                    sh:datatype xsd:string ;
       minItems: 0                                       sh:minCount 0 ;
       maxItems: 10                                      sh:maxCount 10
                                                       ]

                                                     ] ;

*/
:- use_module(library(semweb/rdf11)).
:- use_module(library(pcre)).

:- dynamic cache/2.
:- volatile cache/2.

openapi_read(File, Options0) :-
  path_prefix(File, Prefix),
  debug(_, 'Importing [~w]...', [File]),
  merge_options(Options0, [base_prefix(Prefix)], Options1),
  read_yaml(File, Yaml, Options1),
  merge_options(Options1, [yaml(Yaml)], Options),
  components_clauses(Yaml, Options).

read_yaml(File, Yaml, Options) :-
  once((
    cache(File, Yaml)
  ; setup_call_cleanup(
      ( option(base_path(BasePath), Options),
        absolute_file_name(File, Path, [ relative_to(BasePath),
                                         extensions(['',json,yaml]),
                                         access(read)
                                       ]),
        open(Path, read, In, [encoding(utf8)])
      ),
      yaml_read(In, Yaml),
      ( close(In),
        assertz(cache(File, Yaml))
      )
    )
  )).

clear_cache :-
  retractall(cache(_, _)).

path_prefix(Path, Prefix) :-
  file_base_name(Path, File),
  file_name_extension(NumName, _, File),
  re_matchsub("(?<num>TS\\d*)_(?<name>.*)", NumName, Sub),
  atomic_list_concat(['http://ericsson.com/models/3gpp/', Sub.num, '/', Sub.name, '#'], Prefix).

components_clauses(Term, Options) :-
  Schemas = Term.get(components).get(schemas),
  dict_pairs(Schemas, _, SchemaPairs),
  !,
  maplist(schema_clause(Options), SchemaPairs).
components_clauses(_, _).

schema_clause(Options, Schema-Spec) :-
  option(base_prefix(Prefix), Options),
  atom_concat(Prefix, Schema, Subject),
  type_clause(Subject, _, Spec, Options), !.
schema_clause(_, _).

ref_uri_fragment(Ref, URI, Fragment, Options) :-
  re_matchsub("(?<file>.*)#/components/schemas/(?<name>.*)", Ref, Sub),
  ( Sub.file == ""
  -> option(base_prefix(Prefix), Options),
     option(yaml(Yaml), Options)
  ; path_prefix(Sub.file, Prefix),
    read_yaml(Sub.file, Yaml, Options)
  ),
  atom_concat(Prefix, Sub.name, URI),
  atom_string(Name, Sub.name),
  Fragment = Yaml.get(components/schemas/Name).

property_clause(Subject, Shape, Options, Property-Spec) :-
  option(base_prefix(Prefix), Options),
  atom_concat(Prefix, Property, Predicate),
  ignore((
    nonvar(Subject),
    rdf_assert(Subject, cc:definedProperty, Predicate, Prefix)
  )),
  rdf_assert(Predicate, rdf:type, rdf:'Property', Prefix),
  ignore((
    _{description:Description} :< Spec,
    rdf_assert(Predicate, rdfs:comment, Description, Prefix)
  )),
  rdf_create_bnode(PropertyShape),
  rdf_assert(PropertyShape, rdf:type, sh:'PropertyShape', Prefix),
  rdf_assert(Shape, sh:property, PropertyShape, Prefix),
  rdf_assert(PropertyShape, sh:path, Predicate, Prefix),
  type_clause(Subject, PropertyShape, Spec, Options).

type_clause(Subject, PropertyShape, Spec, Options) :-
  _{'$ref':Ref} :< Spec, !,
  ref_uri_fragment(Ref, Class, Fragment, Options),
  option(base_prefix(Prefix), Options),
  ignore((
    nonvar(Subject),
    rdf_assert(Subject, rdfs:subClassOf, Class, Prefix)
  )),
  ignore((
    nonvar(PropertyShape),
    ( _{type:"object"} :< Fragment
    -> rdf_assert(PropertyShape, sh:class, Class, Prefix)
    ; type_clause(_, PropertyShape, Fragment, Options)
    )
  )).

type_clause(Subject, PropertyShape, Spec, Options) :-
  _{type:"object"} :< Spec, !,
  option(base_prefix(Prefix), Options),
  ignore((
    nonvar(Subject),
    rdf_assert(Subject, rdf:type, rdfs:'Class', Prefix),
    ignore((
      _{description:Description} :< Spec,
      rdf_assert(Subject, rdfs:comment, Description, Prefix)
    ))
  )),
  ignore((
    _{properties:Properties} :< Spec,
    dict_pairs(Properties, _, PropertyPairs),
    ( nonvar(Subject)
    -> atom_concat(Subject, '_Shape', NodeShape),
       rdf_assert(NodeShape, sh:targetClass, Subject, Prefix)
    ; rdf_create_bnode(NodeShape),
      rdf_assert(PropertyShape, sh:node, NodeShape, Prefix)
    ),
    rdf_assert(NodeShape, rdf:type, sh:'NodeShape', Prefix),
    maplist(property_clause(_, NodeShape, Options), PropertyPairs)
  )).

% type_clause(Subject, PropertyShape, Spec, Options) :-
%   _{allOf: AllOf} :< Spec,
%   nonvar(Subject),
%   !,
%   type_branches(Subject, AllOf, Options, List),
%   ignore((
%     nonvar(PropertyShape),
%     option(base_prefix(Prefix), Options),
%     rdf_assert_list(List, RDFList, Prefix),
%     rdf_assert(PropertyShape, sh:and, RDFList, Prefix)
%   )).

type_clause(Subject, PropertyShape, Spec, Options) :-
  _{anyOf: AnyOf} :< Spec, !,
  type_branches(Subject, AnyOf, Options, List),
  ignore((
    nonvar(PropertyShape),
    option(base_prefix(Prefix), Options),
    rdf_assert_list(List, RDFList, Prefix),
    rdf_assert(PropertyShape, sh:or, RDFList, Prefix)
  )).

type_clause(Subject, PropertyShape, Spec, Options) :-
  _{oneOf: OneOf} :< Spec, !,
  type_branches(Subject, OneOf, Options, List),
  ignore((
    nonvar(PropertyShape),
    option(base_prefix(Prefix), Options),
    rdf_assert_list(List, RDFList, Prefix),
    rdf_assert(PropertyShape, sh:xone, RDFList, Prefix)
  )).

type_clause(Subject, PropertyShape, Spec, Options) :-
  _{type:"array", items:Items} :< Spec,
  nonvar(PropertyShape), !,
  option(base_prefix(Prefix), Options),
  ignore((
    _{description: Description} :< Spec,
    rdf_assert(PropertyShape, rdfs:comment, Description, Prefix)
  )),
  rdf_assert(PropertyShape, sh:node, dash:'ListShape', Prefix),
  rdf_create_bnode(ItemShape),
  rdf_assert(PropertyShape, sh:property, ItemShape, Prefix),
  rdf_create_bnode(Rest),
  rdf_assert(Rest, sh:zeroOrMorePath, rdf:rest, Prefix),
  rdf_assert_list([Rest, rdf:first], PathList, Prefix),
  rdf_assert(ItemShape, sh:path, PathList, Prefix),
  ignore((
    _{minItems: MinItems} :< Spec,
    rdf_assert(ItemShape, sh:minCount, MinItems, Prefix)
  )),
  ignore((
    _{maxItems: MaxItems} :< Spec,
    rdf_assert(ItemShape, sh:maxCount, MaxItems, Prefix)
  )),
  type_clause(Subject, ItemShape, Items, Options).

type_clause(_, PropertyShape, Spec, Options) :-
  _{type:"string"} :< Spec,
  nonvar(PropertyShape), !,
  option(base_prefix(Prefix), Options),
  ignore((_{format: "date-time"} :< Spec, rdf_global_id(xsd:dateTime, Type))),
  ignore((_{format: "full-time"} :< Spec, rdf_global_id(xsd:time, Type))),
  ignore((_{format: "date-month"} :< Spec, rdf_global_id(xsd:gMonth, Type))),
  ignore((_{format: "date-mday"} :< Spec, rdf_global_id(xsd:gMonthDay, Type))),
  ( var(Type)
  -> rdf_global_id(xsd:string, Type),
     ignore((
       _{description: Description} :< Spec,
       rdf_assert(PropertyShape, rdfs:comment, Description, Prefix)
     )),
     ignore((
       _{pattern: Pattern} :< Spec,
       rdf_assert(PropertyShape, sh:pattern, Pattern, Prefix)
     )),
     ignore((
       _{minLength: MinLength} :< Spec,
       rdf_assert(PropertyShape, sh:minLength, MinLength, Prefix)
     )),
     ignore((
       _{maxLength: MaxLength} :< Spec,
       rdf_assert(PropertyShape, sh:maxLength, MaxLength, Prefix)
     )),
     ignore((
       _{enum: Enum} :< Spec,
       rdf_assert_list(Enum, EnumList, Prefix),
       rdf_assert(PropertyShape, sh:in, EnumList, Prefix)
     ))
  ; true
  ),
  rdf_assert(PropertyShape, sh:datatype, Type, Prefix).

type_clause(_, PropertyShape, Spec, Options) :-
  once((
    _{type:"integer"} :< Spec,
    rdf_global_id(xsd:integer, Type)
  ; _{type:"number"} :< Spec,
    ( _{format:"float"} :< Spec,
      rdf_global_id(xsd:float, Type)
    ; _{format:"double"} :< Spec,
      rdf_global_id(xsd:double, Type)
    )
  )),
  nonvar(PropertyShape), !,
  option(base_prefix(Prefix), Options),
  rdf_assert(PropertyShape, sh:datatype, Type, Prefix),
  ignore((
    _{description: Description} :< Spec,
    rdf_assert(PropertyShape, rdfs:comment, Description, Prefix)
  )),
  ignore((
    _{minimum: Minimum} :< Spec,
    rdf_assert(PropertyShape, sh:minInclusive, Minimum, Prefix)
  )),
  ignore((
    _{maximum: Maximum} :< Spec,
    rdf_assert(PropertyShape, sh:maxInclusive, Maximum, Prefix)
  )).

type_clause(_, _, _, _).

type_branches(_, [], _, []) :- !.
type_branches(Subject, [Spec|T], Options, [Shape|T2]) :-
  ( var(Subject)
  -> rdf_create_bnode(Shape)
  ; atom_concat(Subject, '_Shape', Shape)
  ),
  ignore((
    _{description:Description} :< Spec,
    option(base_prefix(Prefix), Options),
    rdf_assert(Shape, rdfs:comment, Description, Prefix)
  )),
  type_clause(Subject, Shape, Spec, Options),
  type_branches(Subject, T, Options, T2).
