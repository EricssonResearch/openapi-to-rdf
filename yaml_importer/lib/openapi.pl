:- module(openapi, [ openapi_read/2 ]).

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

openapi_read(Path, Options0) :-
  file_name_extension(FilePath, yaml, Path),
  path_prefix(FilePath, Prefix),
  !,
  debug(_, 'Importing [~w]...', [Path]),
  setup_call_cleanup(
    open(Path, read, In, [encoding(utf8)]),
    yaml_read(In, Term),
    close(In)
  ),
  merge_options(Options0, [base_prefix(Prefix)], Options),
  components_clauses(Term, Options).

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
  _{type:"object"} :< Spec, !,
  option(base_prefix(Prefix), Options),
  atom_concat(Prefix, Schema, Subject),
  rdf_assert(Subject, rdf:type, rdfs:'Class', Prefix),
  ignore((
    _{description:Description} :< Spec,
    rdf_assert(Subject, rdfs:comment, Description, Prefix)
  )),
  ignore((
    _{properties:Properties} :< Spec,
    dict_pairs(Properties, _, PropertyPairs),
    atom_concat(Subject, '_Shape', Shape),
    rdf_assert(Shape, rdf:type, sh:'NodeShape', Prefix),
    rdf_assert(Shape, sh:targetClass, Subject, Prefix),
    maplist(property_clause(Subject, Shape, Options), PropertyPairs)
  )).
schema_clause(Options, Schema-Spec) :-
  _{allOf:AllOf} :< Spec, !,
  option(base_prefix(Prefix), Options),
  atom_concat(Prefix, Schema, Subject),
  maplist(ref_superclass(Subject,Prefix,Options), AllOf).
schema_clause(_, _).

ref_superclass(Subject, Prefix, Options, RefObj) :-
  ( is_dict(RefObj),
    _{'$ref':Ref} :< RefObj,
    ref_uri(Ref, SubClass, Options)
  -> rdf_assert(Subject, rdf:type, rdfs:'Class', Prefix),
     rdf_assert(Subject, rdfs:subClassOf, SubClass, Prefix)
  ; true
  ).

ref_uri(Ref, URI, Options) :-
  re_matchsub("(?<file>.*)#/components/schemas/(?<name>.*)", Ref, Sub),
  ( Sub.file == ""
  -> option(base_prefix(Prefix), Options)
  ; path_prefix(Sub.file, Prefix)
  ),
  atom_concat(Prefix, Sub.name, URI).

property_clause(Subject, Shape, Options, Property-Spec) :-
  option(base_prefix(Prefix), Options),
  atom_concat(Prefix, Property, Predicate),
  rdf_assert(Subject, cc:definedProperty, Predicate, Prefix),
  rdf_assert(Predicate, rdf:type, rdf:'Property', Prefix),
  ignore((
    _{description:Description} :< Spec,
    rdf_assert(Predicate, rdfs:comment, Description, Prefix)
  )),
  rdf_create_bnode(PropertyShape),
  rdf_assert(Shape, sh:property, PropertyShape, Prefix),
  rdf_assert(PropertyShape, sh:path, Predicate, Prefix),
  type_clause(PropertyShape, Spec, Options).

% type_clause(PropertyShape, Spec, Options) :-
%   _{anyOf: AnyOf} :< Spec, !,
%   rdf_create_bnode(PropertyShape),


type_clause(PropertyShape, Spec, Options) :-
  _{type:"array", items:Items} :< Spec, !,
  option(base_prefix(Prefix), Options),
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
  type_clause(ItemShape, Items, Options).

type_clause(PropertyShape, Spec, Options) :-
  _{'$ref':Ref} :< Spec,
  ref_uri(Ref, Class, Options), !,
  option(base_prefix(Prefix), Options),
  rdf_assert(PropertyShape, sh:class, Class, Prefix).

type_clause(PropertyShape, Spec, Options) :-
  _{type:"string"} :< Spec, !,
  option(base_prefix(Prefix), Options),
  ignore((_{format: "date-time"} :< Spec, rdf_global_id(xsd:dateTime, Type))),
  ignore((_{format: "full-time"} :< Spec, rdf_global_id(xsd:time, Type))),
  ignore((_{format: "date-month"} :< Spec, rdf_global_id(xsd:gMonth, Type))),
  ignore((_{format: "date-mday"} :< Spec, rdf_global_id(xsd:gMonthDay, Type))),
  ( var(Type)
  -> rdf_global_id(xsd:string, Type),
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

type_clause(PropertyShape, Spec, Options) :-
  once((
    _{type:"integer"} :< Spec,
    rdf_global_id(xsd:integer, Type)
  ; _{type:"number"} :< Spec,
    ( _{format:"float"} :< Spec,
      rdf_global_id(xsd:float, Type)
    ; _{format:"double"} :< Spec,
      rdf_global_id(xsd:double, Type)
    )
  )), !,
  option(base_prefix(Prefix), Options),
  rdf_assert(PropertyShape, sh:datatype, Type, Prefix),
  ignore((
    _{minimum: Minimum} :< Spec,
    rdf_assert(PropertyShape, sh:minInclusive, Minimum, Prefix)
  )),
  ignore((
    _{maximum: Maximum} :< Spec,
    rdf_assert(PropertyShape, sh:maxInclusive, Maximum, Prefix)
  )).

type_clause(_, _, _).
