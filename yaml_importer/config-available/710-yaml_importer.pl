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

:- module('710-yaml_importer', []).

/** <module> OpenAPI yaml importer
*/

:- use_module(library(semweb/rdf_library)).
:- use_module(library(messages)).
:- use_module(library(yaml_importer)).

:- rdf_attach_library(yaml_importer(rdf)).
:- rdf_load_library(yaml_importer).

:- listen_last(
     reasoner(loaded),
     import_yaml_assets
   ).