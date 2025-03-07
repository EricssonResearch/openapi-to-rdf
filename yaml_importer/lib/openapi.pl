:- module(openapi, [ openapi_read/2,
                     clear_cache/0,
                     openapi_clauses//4 ]).

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

:- dynamic cache/2.
:- volatile cache/2.

openapi_read(File, Term) :-
    file_name_extension(_, yaml, File),
    !,
    ( cache(File, Term)
    -> true
    ; setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        yaml_read(In, Term),
        close(In)
      ),
      assertz(cache(File, Term))
    ).

clear_cache :-
    retractall(cache(_, _)).

openapi_clauses(JSONTerm, Options, Seen0, Seen) -->
    %{ dict_pairs(JSONTerm.paths, _, Paths) },
    root_clause(JSONTerm, Options),
    json_schema_clauses(JSONTerm, Options, Seen0, Seen).

root_clause(_, Options) -->
    { option(server_url(ServerURL), Options),
      !,
      uri_components(ServerURL, Components),
      uri_data(path, Components, Root)
    },
    !,
    [ openapi_root(Root) ].
root_clause(Spec, _Options) -->
    { Spec.get(servers) = [Server|_],
      !,
      uri_components(Server.url, Components),
      uri_data(path, Components, Root)
    },
    [ openapi_root(Root) ].
root_clause(_Spec, _Options) -->
    [ openapi_root('') ].

json_schema_clauses(JSONTerm, Options, Seen0, Seen) -->
    { Schemas = JSONTerm.get(components).get(schemas),
      dict_pairs(Schemas, _, SchemaPairs)
    },
    !,
    schema_clauses(SchemaPairs, Options, Seen0, Seen).
json_schema_clauses(_, _, Seen, Seen) --> [].

schema_clauses([], _, Seen, Seen) --> !.
schema_clauses([H|T], Options, Seen0, Seen) -->
    schema_clause(H, Options, Seen0, Seen1),
    schema_clauses(T, Options, Seen1, Seen).

schema_clause(Schema-Spec, Options, Seen0, Seen) -->
    { json_type(Spec, Type, Options, Seen0, Seen),
      option(base_uri(Base), Options),
      file_directory_name(Base, Dir),
      atomic_list_concat([Dir, '#/components/schemas/', Schema], URL)
    },
    [ openapi:json_schema(URL, Type) ].

json_type(Spec, Type, TypeOpts, Options, Seen0, Seen) :-
    _{'$ref':URLS} :< Spec,
    !,
    option(base_uri(Base), Options),
    uri_normalized(URLS, Base, URL),
    ( get_assoc(URL, Seen0, seen(URL, Type, TypeOpts))
    -> Seen = Seen0
    ; put_assoc(URL, Seen0, seen(URL, Type, TypeOpts), Seen1),
      ( url_yaml(URL, Spec2)
      -> atom_string(NewBase, URL),
         json_type(Spec2, Type, TypeOpts, [base_uri(NewBase)|Options], Seen1, Seen)
      ; Type = url(URL),
        TypeOpts = []
      )
    ).
json_type(Spec, Type, TypeOpts, Options, Seen0, Seen) :-
    json_type(Spec, Type, Options, Seen0, Seen),
    (   Spec.get(nullable) == true
    ->  TypeOpts = [nullable]
    ;   TypeOpts = []
    ).

json_type(Spec, Type, _, Seen, Seen) :-
    _{type:TypeS, format:FormatS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    atom_string(Format, FormatS),
    api_type(Type0, Format, Type1),
    type_restrictions(Spec, Type0, Type1, Type).
json_type(Spec, object(Props), Options, Seen0, Seen) :-
    _{properties:PropSpecs} :< Spec,
    !,
    dict_pairs(PropSpecs, _, Pairs),
    (   maplist(atom_string, Req, Spec.get(required))
    ->  true
    ;   Req = []
    ),
    schema_properties(Req, Options, Pairs, Props0, Seen0, Seen),
    sort(Props0, Props).
json_type(Spec, array(Type, Opts), Options, Seen0, Seen) :-
    _{type:"array", items:IType} :< Spec,
    !,
    array_restrictions(Spec, Opts),
    json_type(IType, Type, Options, Seen0, Seen).
json_type(Spec, oneOf(Types), Options, Seen0, Seen) :-
    _{oneOf:List} :< Spec,
    !,
    json_types(List, Types, Options, Seen0, Seen).
json_type(Spec, allOf(Types), Options, Seen0, Seen) :-
    _{allOf:List} :< Spec,
    !,
    json_types(List, Types, Options, Seen0, Seen).
json_type(Spec, anyOf(Types), Options, Seen0, Seen) :-
    _{anyOf:List} :< Spec,
    !,
    json_types(List, Types, Options, Seen0, Seen).
json_type(Spec, not(Type), Options, Seen0, Seen) :-
    _{not:NSpec} :< Spec,
    !,
    json_type(NSpec, Type, Options, Seen0, Seen).
json_type(Spec, object, _Options, Seen, Seen) :-
    _{type:"object"} :< Spec,
    !.
json_type(Spec, enum(Values, CaseSensitive, Case), Options, Seen, Seen) :-
    _{type:"string", enum:ValuesS} :< Spec,
    !,
    option(enum_case_sensitive(CaseSensitive), Options, true),
    option(enum_case(Case), Options, preserve),
    maplist(atom_string, Values, ValuesS).
json_type(Spec, Type, _, Seen, Seen) :-
    _{type:TypeS} :< Spec,
    !,
    atom_string(Type0, TypeS),
    api_type(Type0, -, Type1),
    type_restrictions(Spec, Type0, Type1, Type).
json_type(Spec, Type, Options, Seen0, Seen) :-
    _{'$ref':URLS} :< Spec,
    !,
    option(base_uri(Base), Options),
    uri_normalized(URLS, Base, URL),
    ( get_assoc(URL, Seen0, seen(URL, Type, _))
    -> Seen = Seen0
    ; put_assoc(URL, Seen0, seen(URL, Type, _), Seen1),
      ( url_yaml(URL, Spec2)
      -> atom_string(NewBase, URL),
         json_type(Spec2, Type, [base_uri(NewBase)|Options], Seen1, Seen)
      ; Type = url(URL)
      )
    ).
json_type(_{properties:_{}}, Type, _Options, Seen, Seen) :-
    !,
    Type = (-).
json_type(_Spec, 'InvalidType', _Options, Seen, Seen) :- % FIXME
    true.

schema_properties(_, _, [], [], Seen, Seen) :- !.
schema_properties(Reqs, Options, [H|T], [H2|T2], Seen0, Seen) :-
    ( schema_property(Reqs, Options, H, H2, Seen0, Seen1)
    -> schema_properties(Reqs, Options, T, T2, Seen1, Seen)
    ; print_message(error, openapi(failed, 'failed to process [~w]', [H])),
      fail
    ).

schema_property(Reqs, Options, Name-Spec, p(Name, Type, TypeOpts), Seen0, Seen) :-
    (   memberchk(Name, Reqs)
    ->  TypeOpts = [ required | TypeOpts1 ]
    ;   TypeOpts = TypeOpts1
    ),
    json_type(Spec, Type, TypeOpts1, Options, Seen0, Seen).

json_types([], [], _, Seen, Seen) :- !.
json_types([H|T], [H2|T2], Options, Seen0, Seen) :-
    json_type(H, H2, Options, Seen0, Seen1),
    json_types(T, T2, Options, Seen1, Seen).

api_type(Type, Format, TypeID) :-
    api_type(_Name, Type, Format, TypeID), !.
api_type(string, Format, string) :-
    !,
    print_message(warning, openapi(unknown_string_format, Format)).
api_type(Type, Format, _TypeID) :-
    print_message(error, openapi(unknown_type, Type, Format)),
    fail.

api_type(integer,  integer,    int32,       int32).
api_type(long,     integer,    int64,       int64).
api_type(long,     integer,    -,           integer).
api_type(float,    number,     float,       float).
api_type(double,   number,     double,      float).
api_type(double,   number,     -,           float).
api_type(string,   string,     -,           string).
api_type(byte,     string,     byte,        base64).
api_type(binary,   string,     binary,      binary).
api_type(boolean,  boolean,    -,           boolean).
api_type(date,     string,     date,        date).
api_type(dateTime, string,     'date-time', date_time).
api_type(password, string,     password,    password).
api_type(string,   string,     string,      string). % Not in OAS
api_type(uri,      string,     uri,         uri).    % Not in OAS
api_type(uuid,     string,     uuid,        uuid).   % Not in OAS

array_restrictions(Spec, Options) :-
    findall(Opt, array_restriction(Spec, Opt), Options).

array_restriction(Spec, min_items(Min)) :-
    Min = Spec.get(minItems).
array_restriction(Spec, max_items(Max)) :-
    Max = Spec.get(maxItems).
array_restriction(Spec, unique_items(true)) :-
    true == Spec.get(uniqueItems).

type_restrictions(Spec, Type0, Type1, Type) :-
    numeric_type(Type0),
    !,
    (   _{minimum:Min, maximum:Max} :< Spec
    ->  Type = numeric(Type1, domain(between(Min,Max), ExclMin-ExclMax, MultipleOf))
    ;   _{minimum:Min} :< Spec
    ->  Type = numeric(Type1, domain(min(Min), ExclMin, MultipleOf))
    ;   _{maximum:Max} :< Spec
    ->  Type = numeric(Type1, domain(max(Max), ExclMax, MultipleOf))
    ;   Type = Type1
    ),
    (   _{ exclusiveMinimum: ExclMin} :< Spec
    ->  true
    ;   ExclMin = false
    ),
    (   _{ exclusiveMaximum: ExclMax} :< Spec
    ->  true
    ;   ExclMax = false
    ),
    (   _{ multipleOf: MultipleOf} :< Spec
    ->  true
    ;   MultipleOf = (-)
    ).
type_restrictions(Spec, string, string, Type) :-
    setof(Restrict, string_restriction(Spec, Restrict), Restrictions),
    !,
    Type = string(Restrictions).
type_restrictions(_, _Type0, Type, Type).

numeric_type(integer).
numeric_type(number).

string_restriction(Spec, max_length(Len)) :-
    Len = Spec.get(maxLength).
string_restriction(Spec, min_length(Len)) :-
    Len = Spec.get(minLength).
string_restriction(Spec, pattern(Regex)) :-
    atom_string(Regex, Spec.get(pattern)).

url_yaml(URL, Yaml) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, file),
    uri_data(path, Components, FileEnc),
    uri_data(fragment, Components, Fragment),
    uri_encoded(path, File, FileEnc),
    openapi_read(File, Yaml0),
    (   var(Fragment)
    ->  Yaml = Yaml0
    ;   atomic_list_concat(Segments, /, Fragment),
        yaml_subdoc(Segments, Yaml0, Yaml)
    ).

yaml_subdoc([], Doc, Doc) :- !.
yaml_subdoc([H|T], Doc, Sub) :-
    (   (H == '' ; H == '#')
    ->  Sub0 = Doc
    ;   Sub0 = Doc.H
    ),
    yaml_subdoc(T, Sub0, Sub).

:- multifile
    prolog:message//1.

prolog:message(openapi(failed, Culprit, Params)) -->
    { format(atom(C), Culprit, Params) },
    [ 'OpenAPI: ~w'-[C] ].

prolog:message(openapi(unknown_type, Type, Format)) -->
    [ 'OpenAPI: unrecognized type `~p` with format `~p`'-[Type, Format] ].
prolog:message(openapi(unknown_string_format, Format)) -->
    [ 'OpenAPI: Using plain "string" for string with format `~p`'-[Format] ].
