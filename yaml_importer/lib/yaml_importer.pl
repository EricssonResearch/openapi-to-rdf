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

import_yaml_assets :-
  catch((
    atom_concat('cpack/yaml_importer/assets/', '*.yaml', Search),
    expand_file_name(Search, Files),
    forall(
      ( member(Path, Files),
        exists_file(Path)
      ),
      import_yaml_file(Path)
    )
  ), _, true).

import_yaml_file(File) :-
  debug(_, 'Importing [~w]...', [File]).
  %yaml_read(File, Dom),
  %print_term(Dom, []).