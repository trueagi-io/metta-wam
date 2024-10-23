:- module(lsp_metta_colours, [
                        token_types/1,
                        token_modifiers/1]).

% these are LSP token types/modifiers.

token_types([namespace,
             type,
             class,
             enum,
             interface,
             struct,
             typeParameter,
             parameter,
             variable,
             property,
             enumMember,
             event,
             function,
             member,
             macro,
             keyword,
             modifier,
             comment,
             string,
             number,
             regexp,
             operator
            ]).
token_modifiers([declaration,
                 definition,
                 readonly,
                 static,
                 deprecated,
                 abstract,
                 async,
                 modification,
                 documentation,
                 defaultLibrary
                ]).

