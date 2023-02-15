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
    % St = new_server(ServerAtom),
    % io:format("Server: ~p, Channels: ~p", [St#server_state.server, St#server_state.channels]),
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
    Channels = State#server_state.channels,
    case lists:member(Channel, Channels) of
        true  -> 
            % request join to the channel
            Result = genserver:request(list_to_atom(Channel), {join, Client}),
            {reply, Result, State};

        false -> 
            % spawns process for channel with Client as a member.
            spawn(genserver, start, [list_to_atom(Channel), new_channel(Channel, Client), fun handle_channel/2]),
            {reply, joined, State#server_state{channels = Channels ++ [Channel]}}
    end;

handle_server(State, {leave, Client, Channel}) ->
    Channels = State#server_state.channels,
    case lists:member(Channel, Channels) of
        true -> 
            Result = genserver:request(list_to_atom(Channel), {leave, Client}),
            {reply, Result, State};
        false ->
            {reply, , State}
    end.



handle_channel(C_St, {join, Client}) ->
    Members = C_St#channel_state.members,
    case lists:member(Client, Members) of
        true  -> {reply, failed, C_St};
        false -> {reply, joined, C_St#channel_state{members = Members ++ [Client]}}
    end;

handle_channel(C_St, {leave, Client}) ->
    Members = C_St#channel_state.members,
    case lists:member(Client, Members) of
        false -> {reply, failed, C_St};
        true  -> {reply, success, C_St#channel_state{members = lists:delete(Client, Members)}} 
    end.