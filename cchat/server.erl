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

% handle_server/2 handles each kind of request sent to the server from Client
% Parameters:
%   - the current state of the server (State)
%   - request data from client.
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to the Client
%   - NewState is the updated state of the server


% This one handles all join requests.
handle_server(State, {join, Client, Channel}) ->
    Channels = State#server_state.channels,
    % Checks if the channel exists already.
    case lists:member(Channel, Channels) of
        true  -> 
            % Channel already exists,
            % Sends request to the channel to join it.
            Result = genserver:request(list_to_atom(Channel), {join, Client}),
            {reply, Result, State};

        false -> 
            % Initiates channel and spawns process for it.
            spawn(genserver, start, [list_to_atom(Channel), new_channel(Channel, Client), fun handle_channel/2]),
            % Sends response that user successfully joined channel
            {reply, joined, State#server_state{channels = Channels ++ [Channel]}}
    end;

% Handles all leave requests sent to server.
handle_server(State, {leave, Client, Channel}) ->
    Channels = State#server_state.channels,
    % Checks if specified channel exists.
    case lists:member(Channel, Channels) of
        true -> 
            % Sends request to channel Pid to leave the channel
            Result = genserver:request(list_to_atom(Channel), {leave, Client}),
            {reply, Result, State};
        false ->
            % Sends response that process failed since the channel didn't exist.
            {reply, failed, State}
    end.

% handle_channel/2 handles each kind of request sent to a channels, either from server or a client.
% Parameters:
%   - the current state of the channel (C_St)
%   - request data.
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to the Client/Server
%   - NewState is the updated state of the channel

% Handles all join requests sent to channel
handle_channel(C_St, {join, Client}) -> 
    Members = C_St#channel_state.members,
    % Checks if the user is already a member.
    case lists:member(Client, Members) of
        % is a member -> fail
        true  -> {reply, failed, C_St};
        % not a member -> add them to the channel
        false -> {reply, joined, C_St#channel_state{members = Members ++ [Client]}}
    end;

% Handles all leave requests sent to channel
handle_channel(C_St, {leave, Client}) ->
    Members = C_St#channel_state.members,
    % Checks if the user is a member.
    case lists:member(Client, Members) of
        % not a member -> fail
        false -> {reply, failed, C_St};
        % is a member -> remove them from the channel
        true  -> {reply, success, C_St#channel_state{members = lists:delete(Client, Members)}} 
    end;

% Handles all message requests sent to channel
handle_channel(C_St, {message_send, Msg, Nick, Client}) ->
    Members = C_St#channel_state.members,
    Channel = C_St#channel_state.name,
    % Checks if user is a member of the channel
    case lists:member(Client, Members) of
        false ->
            {reply, {error, user_not_joined, "User is not in the channel."}, C_St};
        true  ->
            % Remove user from list of recipuents
            % Don't want to send a message to themselves.
            Recipients = lists:delete(Client, Members),
            % Create a function that sends a message_receive request to a client.
            Fun = fun(P) ->
                    spawn(genserver, request, [P, {message_receive, Channel, Nick, Msg}])
                end,
            % Sends the request to all recipients.
            lists:foreach(Fun, Recipients),
            {reply, ok, C_St}
    end.    