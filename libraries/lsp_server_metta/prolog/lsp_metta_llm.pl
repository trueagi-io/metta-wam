:- module(lsp_metta_llm, [ request_code_comment/2 ]).

/** <module> lsp_metta_llm
 * This module implements an interface to an LLM over an HTTP API to
   provide generated code comments explaining metta code.

  To run a local model, the recommended method is to use Ollama and
  use the included Modelfile to run an instance of llama with a system
  prompt including documentation on the metta programming language.
  You can do this by installing ollama and running the command
 $ ollama create llama3-metta -f libraries/lsp_server_metta/Modelfile

  @author James Cash
 */

:- include(lsp_metta_include).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

llm_http_api(Uri) :-
    getenv('METTA_LLM_URL', Uri), !.
llm_http_api('https://api.openai.com/v1/responses').

llm_model(Model) :-
    getenv('METTA_LLM_MODEL', Model), !.
llm_model('gpt-4o').

llm_http_auth_key(Key) :- getenv('METTA_LLM_KEY', Key).

using_custom_endpoint :-
    getenv('METTA_LLM_URL', _).

request_code_comment(Code, Commented) :-
    llm_http_auth_key(Key),
    llm_http_api(Uri),
    llm_model(Model),
    string_concat("Task: Please comment this source code by first outputting a comment describing the overall purpose of the code or function. Then, line by line describe what it is doing. Put each comment right above the line it is commenting. Improve the formatting when it makes sense to break up lines but don't add too much vertical space. Return only a text block that will replace exactly the block I just gave you. Do not include any other formatting markers such as markdown code fences; only the original code and the interleaved comments should be output. Be sure to include all the code and do not make any changes to the functionality, only add comments. \n\n Code:\n", Code, Prompt),
    ( using_custom_endpoint
    % assuming if a URL has been set, it's Ollama...make this configurable?
    -> ReqBody = _{model: Model,
                   stream: false,
                   prompt: Prompt}
    ;  ReqBody = _{model: Model,
                   % For OpenAI, include the system prompt for the
                   % custom model with all the metta docs?
                   input: Prompt} ),
    http_post(Uri,
              json(ReqBody),
              Resp,
              [authorization(bearer(Key)), json_object(dict)]),
    Commented = Resp.response.

/*
lsp_metta_llm:request_code_comment("(: (do_quoted) (-> Expression Atom))
(= (do_quoted $exp)
  (if (== $exp ())
    empty
    (let () (unquote (car-atom $exp)) (do_quoted (cdr-atom $exp)))))", Resp).

*/
