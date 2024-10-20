:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

:- ['src/prolog/text_utils.pro'].

lp_base_url('https://api.openai.com').

init_api_key:-
   getenv('OPENAI_API_KEY' ,Key),
   create_prolog_flag(trace_lp, true, [type(atom)]),
   create_prolog_flag(log_lp, false, [type(atom)]),
   create_prolog_flag(lp_key, Key, [type(atom)]).

extract_model_ids(json(Objects), ModelIDs) :-
    member(data = Models, Objects),
    findall(ID, (member(json(ModelSpecs), Models), member(id = ID, ModelSpecs)), ModelIDs).

list_models(Models):-
    current_prolog_flag(lp_key, Key),
    
    lp_base_url(BaseURL),
    format(atom(URL), '~w/v1/models', BaseURL),
    http_get(URL, Models, [authorization(bearer(Key)), application/json]).

create_message(Role, Content, Message) :-
    Message = json([role = Role, content = Content]).

create_system_message(Message) :-
    system_prompt(SystemPrompt),
    create_message('system', SystemPrompt, Message).

chat_with_lp(Messages, Response) :-
    chat_with_lp('gpt-4', Messages, [temperature = 0], 3, false, Response).

chat_with_lp(Model, Messages, Options, Attempts, Trace, Response) :-
    Attempts > 0,

    current_prolog_flag(lp_key,Key),
    atom_json_term(Request, json([model = Model, messages = Messages | Options]), []),

    format(atom(FormattedRequest), '~w', Request),
    (   Trace = true
    ->  format('~n~n~w~n~n', FormattedRequest)
    ; true
    ),

    lp_base_url(BaseURL),
    format(atom(URL), '~w/v1/chat/completions', BaseURL),

    catch(
        (
            http_post(
                URL, 
                atom(application/json, FormattedRequest), 
                Result,
                [authorization(bearer(Key)), application/json]),
            Response = Result,
            (   Trace = true
            ->  format('~n~w~n', Result)
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

response_message(json(Response), Message) :-
    member(choices = [json(Choice0) | _], Response),
    member(message = Message, Choice0), !.

response_content(Response, Content) :-
    response_message(Response, json(Message)),
    member(content = Content, Message), !.

response_size(json(Response), Size) :-
    member(usage = json(Usage), Response),
    member(total_tokens = Size, Usage), !.

run_CoT(Prompts, Messages, Responses) :-
    current_prolog_flag(trace_lp, Trace),
    run_CoT('gpt-4', Prompts, [temperature = 0], Trace, Messages, Responses).

run_CoT(Model, Prompts, Messages, Responses) :-
    current_prolog_flag(trace_lp, Trace),
    run_CoT(Model, Prompts, [temperature = 0], Trace, Messages, Responses).

run_CoT(Model, Prompts, Options, Trace, Messages, Responses) :-
    create_system_message(SystemMessage),
    InitialMessages = [SystemMessage],
    run_CoT(Model, Prompts, Options, Trace, InitialMessages, Messages, [], Responses).

run_CoT(_Model, [], _Options, _Trace, Messages, Messages, Responses, Responses) :- !.

run_CoT(Model, [Prompt | Prompts], Options, Trace, CurrentMessages, Messages, CurrentResponses, Responses) :-
    log_message(Prompt), 

    create_message('user', Prompt, Message),
    append(CurrentMessages, [Message], NewMessages),
    
    chat_with_lp(Model, NewMessages, Options, 3, Trace, Response),
    
    response_message(Response, ResponseMessage),
    append(NewMessages, [ResponseMessage], NewMessages1),
    append(CurrentResponses, [Response], NewResponses),
    
    run_CoT(Model, Prompts, Options, Trace, NewMessages1, Messages, NewResponses, Responses).
