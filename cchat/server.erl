-module(server).
-export([start/1,stop/1]).

-record(server_state, {
    server,
    channels
}).

new_server(ServerAtom) ->
    #server_state{
        server = ServerAtom,
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    % not_implemented.
    spawn(genserver, start, [ServerAtom, new_server(ServerAtom), fun handle_server/2]).
    % genserver:start(ServerAtom, #init_state{}, fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    % not_implemented.
    % genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).

handle_server(State, {join, Client, Channel}) ->
    io:format("handle_server: join ~n");

handle_server(State, {leave, Client, Channel}) ->
    io:format("handle_server: leave ~n").
