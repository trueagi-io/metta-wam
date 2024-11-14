/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%********************************************************************************************* 
% PROGRAM FUNCTION: configures environment settings and loads core modules for interfacing with 
% FlyBase, a biological database for fruit fly genetics research.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set the encoding of the source file to ISO Latin 1, a single-byte character encoding.
% This ensures compatibility with Latin-1 character encoding standards, commonly used
% in Western European languages.
:- encoding(iso_latin_1).

% Immediately flush the output buffer, which forces any data currently in the output
% buffer to be written out. This can be useful to ensure that log or debugging messages
% are displayed promptly without delay.
:- flush_output.

% Set the 'RUST_BACKTRACE' environment variable to 'full'. This enables detailed
% backtraces when a Rust program encounters an error, which can aid in debugging.
% Although Rust is not directly involved in Prolog, this setup might be relevant if
% external Rust components are integrated with this Prolog environment.
:- setenv('RUST_BACKTRACE', full).

% Load the `flybase_main` module if it is not already loaded. This module is expected
% to contain the main logic or foundational predicates for FlyBase operations.
:- ensure_loaded(flybase_main).

% Load the `flybase_json` module if it is not already loaded. This module
% provides predicates for handling JSON data specific to FlyBase.
:- ensure_loaded(flybase_json).

% Load the `flybase_obo` module if it is not already loaded. This module might
% contain functionality for working with OBO (Open Biomedical Ontologies) data format,
% commonly used in biological data.
:- ensure_loaded(flybase_obo).

% This line is commented out, potentially because `swi_support` was temporarily
% disabled or is not required in the current setup. If re-enabled, it would load
% support utilities specific to SWI-Prolog that might assist with system or
% compatibility features.
% :- ensure_loaded(swi_support).
