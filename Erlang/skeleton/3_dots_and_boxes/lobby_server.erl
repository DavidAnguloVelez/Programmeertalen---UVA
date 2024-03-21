%   David Angulo Velez
%   UvaID - 14977524
%   Bachelor Informatica

%   Het volgende programma implementeerd de basis van een lobby server voor het
%   de game servers van het spel 'kamertje verhuur'.

-module(lobby_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/0, handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, new_game/3, games/0]).

% You need to implement the call for this to work.
% Should respond with {ok, PidOfCreatedGame}
new_game(W, H, Players) ->
    gen_server:call(lobby_server, {new_game, W, H, Players}).

games() ->
    start(),
    gen_server:call(lobby_server, games).

% Call this when you want to start the lobby server.
start() ->
    gen_server:start({local, lobby_server}, lobby_server, [], []).

init([]) ->
    {ok, []}.

% TODO: add handle_call to make new_game/3 work.
handle_call({new_game, W, H, Players}, _From, Games) ->
    {ok, PidNewGame} = game_server:start_link({W, H, Players}),
    erlang:monitor(process, PidNewGame),
    NewGames = Games ++ [PidNewGame],
    {reply, {ok, PidNewGame}, NewGames};

handle_call(games , _From, Games) ->
    {reply, Games, Games}.

handle_info({'DOWN', _MonitorRef, process, GamePid, _Reason}, Games) ->
    NewGames = Games -- [GamePid],
    {noreply, NewGames}.

% Required for gen_server behaviour.
% Normally you would implement this to,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
