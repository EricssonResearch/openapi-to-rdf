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
  RelativePath = 'cpack/yaml_importer/assets/',
  atom_concat(RelativePath, '*.yaml', Search),
  expand_file_name(Search, Files),
  %Files = ['cpack/yaml_importer/assets/TS29520_Nnwdaf_AnalyticsInfo.yaml'],
  absolute_file_name(RelativePath, BasePath, [relative_to(Dir)]),
  maplist(import_yaml_file([base_path(BasePath)]), Files),
  clear_cache.

import_yaml_file(Options, Path) :-
  file_base_name(Path, File),
  openapi_read(File, Options).
