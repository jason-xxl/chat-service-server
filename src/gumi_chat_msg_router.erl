-module(gumi_chat_msg_router).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-export([send/3, login/2, logout/1, broadcast/2]).

-define(SERVER, global:whereis_name(?MODULE)).

% will hold bidirectional mapping between id <--> pid
-record(state, {pid2id, id2pid}).

start_link() ->
	io:format("router started.\n",[]),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

% sends Msg to anyone logged in as Id
send(Msg, User_id_from, User_id_to) ->
    gen_server:call(?SERVER, {send_msg, Msg, User_id_from, User_id_to}).

broadcast(Msg, User_id_from) ->
    gen_server:call(?SERVER, {broadcast_msg, Msg, User_id_from}).

login(Id, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {login, Id, Pid}).

logout(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {logout, Pid}).

%%

init([]) ->
	io:format("router was initiated.\n",[]),
    % set this so we can catch death of logged in pids:
    process_flag(trap_exit, true),
    % use ets for routing tables
    {ok, #state{
                pid2id = ets:new(?MODULE, [bag]),
                id2pid = ets:new(?MODULE, [bag])
               }
    }.

handle_call({login, Id, Pid}, _From, State) when is_pid(Pid) ->
    ets:insert(State#state.pid2id, {Pid, Id}),
    ets:insert(State#state.id2pid, {Id, Pid}),
    link(Pid), % tell us if they exit, so we can log them out
    io:format("~w logged in as ~w\n",[Pid, Id]),
    {reply, ok, State};

handle_call({logout, Pid}, _From, State) when is_pid(Pid) ->
    unlink(Pid),
    PidRows = ets:lookup(State#state.pid2id, Pid),
    case PidRows of
        [] ->
            ok;
        _ ->
            IdRows = [ {I,P} || {P,I} <- PidRows ], % invert tuples
            % delete all pid->id entries
            ets:delete(State#state.pid2id, Pid),
            % and all id->pid
            [ ets:delete_object(State#state.id2pid, Obj) || Obj <- IdRows ]
    end,
    io:format("pid ~w logged out\n",[Pid]),
    {reply, ok, State};


handle_call({send_msg, Msg, User_id_from, User_id_to}, _From, State) ->
    if 
        User_id_from == User_id_to->
            {reply, ok, State};
        User_id_from /= User_id_to->
            Pids = [ P || { _User_id_to, P } <- ets:lookup(State#state.id2pid, User_id_to) ],
            User_existence = length(Pids),
            if 
                User_existence > 0 ->
                    M = {text, Msg, User_id_from},
                    [ Pid ! M || Pid <- Pids ],
                    {reply, ok, State};
                true ->
                    Pids_from = [ P || { _User_id_from, P } <- ets:lookup(State#state.id2pid, User_id_from) ],
                    M = {text, Msg, User_id_from},
                    [ Pid ! M || Pid <- Pids_from ],
                    {reply, ok, State}
            end;
        true->
            {reply, ok, State}
    end;       



handle_call({broadcast_msg, Msg, User_id_from}, _From, State) ->
    % get pids who are logged in as this Id
    Msg1={text, Msg, User_id_from},

    Send_func= fun(Id2pid_tuple, Count)->
        io:format('Id2pid_tuple: ~w',[Count]),
        {_User_id_to,Pid}=Id2pid_tuple,
        Pid ! Msg1,
        Count1=Count+1,
        Count1
    end,

    Total_sent=ets:foldl(Send_func,0,State#state.id2pid),

    %Pids = [ P || { _User_id, P } <- ets:lookup(State#state.id2pid, User_id) ],
    % send Msg to them all
    io:format('Total sent: ~w',[Total_sent]),
    {reply, ok, State};

handle_call( Msg , _From, State) ->
    io:format('Unknown call: ~p',[Msg]),
    {reply, ok, State}.



% handle death and cleanup of logged in processes
handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, _Why} ->
            % force logout:
            handle_call({logout, Pid}, blah, State); 
        _ ->
            io:format("Caught unhandled message: ~p\n", [Info])
    end,
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.