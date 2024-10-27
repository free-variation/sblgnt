:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

:- ['src/prolog/text_utils.pro'].

lp_base_url('https://api.anthropic.com').

init_api_key:-
   getenv('ANTHROPIC_API_KEY' ,Key),
   create_prolog_flag(trace_lp, true, [type(atom)]),
   create_prolog_flag(log_lp, false, [type(atom)]),
   create_prolog_flag(lp_key, Key, [type(atom)]).

lp_api_version('2023-06-01').


extract_model_ids(json(Objects), ModelIDs) :-
    member(data = Models, Objects),
    findall(ID, (member(json(ModelSpecs), Models), member(id = ID, ModelSpecs)), ModelIDs).

list_models(Models):-
    current_prolog_flag(lp_key, Key),
    
    lp_base_url(BaseURL),
    format(atom(URL), '~w/v1/models', BaseURL),
    http_get(URL, Models, [authorization(bearer(Key)), application/json]).

create_message(Role, Content, Message) :-
    atom_string(Role, RoleString),
    atom_string(Content, ContentString),
    Message = json([role = RoleString, content = ContentString]).


chat_with_lp(Messages, Response) :-
    chat_with_lp('gpt-4', Messages, [temperature = 0], 3, false, Response).

chat_with_lp(Model, Messages, Options, Attempts, Trace, Response) :-
    Attempts > 0,

    current_prolog_flag(lp_key, Key),
    lp_api_version(APIVersion),
    system_message(SystemMessage),

    Request = json([
        model = Model, 
        max_tokens = 4096,
        system = SystemMessage,
        messages = Messages 
        | Options
    ]),

    (   Trace = true
    ->  format('~n~n~p  ~n~n', Request)
    ;   true
    ),

    lp_base_url(BaseURL),
    format(atom(URL), '~w/v1/messages', BaseURL),

    catch(
        (
            http_post(
                URL, 
                json(Request),
                Result,
                [
                    request_header('x-api-key' = Key),
                    request_header('anthropic-version' = APIVersion),
                    status_code(Code)    
                ]
            ),
            Response = Result,
            (   Trace = true
            ->  format('Status: ~w~nResult: ~w~n', [Code, Result])
            ;   true
            )
        ),
        error(Error, Context),
        (
            format('~w ~w~n', [Error, Context]),
            sleep(10),
            OneFewerAttempts is Attempts - 1,
            chat_with_lp(Model, Messages, Options, OneFewerAttempts, Trace, Response)
        )
    ).

response_content(json(Response), Content) :-
    member(content = [json(ContentObj) | _], Response),
    member(text = Content, ContentObj), !.

% Get additional response metadata if needed
response_metadata(json(Response), Model, Role, Type) :-
    member(model = Model, Response),
    member(role = Role, Response),
    member(type = Type, Response), !.

response_usage(json(Response), InputTokens, OutputTokens) :-
    member(usage = json(Usage), Response),
    member(input_tokens = InputTokens, Usage),
    member(output_tokens = OutputTokens, Usage), !.

response_total_tokens(Response, Total) :-
    response_usage(Response, Input, Output),
    Total is Input + Output.


run_CoT(Prompts, Messages, Responses) :-
    current_prolog_flag(trace_lp, Trace),
    run_CoT('gpt-4', Prompts, [temperature = 0], Trace, Messages, Responses).

run_CoT(Model, Prompts, Messages, Responses) :-
    current_prolog_flag(trace_lp, Trace),
    run_CoT(Model, Prompts, [temperature = 0], Trace, Messages, Responses).

run_CoT(Model, Prompts, Options, Trace, Messages, Responses) :-
    run_CoT(Model, Prompts, Options, Trace, [], Messages, [], Responses).

run_CoT(_Model, [], _Options, _Trace, Messages, Messages, Responses, Responses) :- !.

run_CoT(Model, [Prompt | Prompts], Options, Trace, CurrentMessages, Messages, CurrentResponses, Responses) :-
    create_message('user', Prompt, Message),
    append(CurrentMessages, [Message], NewMessages),
    
    chat_with_lp(Model, NewMessages, Options, 3, Trace, Response),
    
    response_content(Response, Content),
    create_message("assistant", Content, ResponseMessage),
    append(NewMessages, [ResponseMessage], NewMessages1),
    append(CurrentResponses, [Response], NewResponses),
    
    run_CoT(Model, Prompts, Options, Trace, NewMessages1, Messages, NewResponses, Responses).

chat_with_lp_test :-
    current_prolog_flag(lp_key, Key),
    
    % Minimal request structure matching API docs exactly
    Request = _{
        model: "claude-3-5-sonnet-20241022",
        messages: [
            _{role: "user", content: "Hello world"}
        ],
        max_tokens: 4096
    },
    
    % Print request for verification
    json_write(current_output, Request, [width(0)]),
    
    % Make request
    http_post(
        'https://api.anthropic.com/v1/messages',
        json(Request),
        Response,
        [
            request_header('x-api-key' = Key),
            request_header('anthropic-version' = '2023-06-01'),
            status_code(Code)
        ]
    ),
    format('~nStatus: ~w~nResponse: ~w~n', [Code, Response]).
