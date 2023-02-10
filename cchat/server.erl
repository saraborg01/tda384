-module(server).
-export([start/1,stop/1]).

-record(server_state, {
    server,
    channels
}).

-record(channel_state, {
    name,
    members}).

new_server(ServerAtom) ->
    #server_state{
        server = ServerAtom,
        channels = []
    }.

new_channel(Channel, MemberPid) -> 
    #channel_state{
        name = Channel,
        members = [MemberPid]
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    % not_implemented.
    St = new_server(ServerAtom),
    io:format("Server: ~p, Channels: ~p", [St#server_state.server, St#server_state.channels]),
    spawn(genserver, start, [ServerAtom, St, fun handle_server/2]).
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
    io:format(Channel),
    io:format(list_to_atom(Channel)),
    case lists:member(Channel, State#server_state.channels) of
        true  -> 
            % add member to existing channel
            add_member(Channel, Client);
        false -> 
            St = new_channel(Channel, Client),
            io:format("Channel: ~p, Members: ~p", [St#channel_state.name, St#channel_state.members]),
            
            spawn(genserver, start, [list_to_atom(Channel), St, fun handle_channel/2]),
            %% add channel to channels
            io:format("channel created ~n"),
            add_channel(State,Channel)
    end;

handle_server(State, {leave, Client, Channel}) ->
    io:format("handle_server: leave ~n").

% Adds channel to specified server
add_channel(State, Channel) ->
    C = State#server_state.channels,
    State#server_state{channels = [Channel|C]},
    io:format("channel added ~n").

% Adds client to specified channel
add_member(C_St, Client) ->
    M = C_St#channel_state.members,
    C_St#channel_state{members = [Client|M]}.

handle_channel(C_St, Data) ->
    io:format("Handle channel ~n").