/*
Copyright 2025 Ericsson AB

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

:- module(yaml_importer, [import_yaml_assets/0]).

:- use_module(library(yaml)).
:- use_module(library(openapi)).

import_yaml_assets :-
  ( prolog_load_context(directory, Dir)
  -> true
  ; Dir = '.'
  ),
  %prolog_ide(thread_monitor),
  atom_concat('cpack/yaml_importer/assets/', '*.yaml', Search),
  expand_file_name(Search, Files),
  %Files = ['cpack/yaml_importer/assets/TS29520_Nnwdaf_AnalyticsInfo.yaml'],
  empty_assoc(E),
  import_yaml_files(Files, Dir, [], E, Seen),
  clear_cache,
  assoc_to_keys(Seen, Keys),
  sort(Keys, Sorted),
  print_term(Sorted, []).

import_yaml_files([], _, _, Seen, Seen) :- !.
import_yaml_files([File|T], Dir, Options, Seen0, Seen) :-
  import_yaml_file(File, Dir, Options, Seen0, Seen1),
  import_yaml_files(T, Dir, Options, Seen1, Seen).

import_yaml_file(File, Dir, Options0, Seen0, Seen) :-
  absolute_file_name(File, Path,
                     [ relative_to(Dir),
                       extensions(['',json,yaml]),
                       access(read)
                     ]),
  debug(_, 'Importing [~w]...', [Path]),
  uri_file_name(BaseURI, Path),
  openapi_read(Path, Spec),
  merge_options(Options0, [base_uri(BaseURI)], Options),
  phrase(openapi_clauses(Spec, Options, Seen0, Seen), _Clauses).
