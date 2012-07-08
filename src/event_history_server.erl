%%% -------------------------------------------------------------------
%%% Author  : andrew
%%% Description :
%%%
%%% Created : Jul 8, 2012
%%% -------------------------------------------------------------------
-module(event_history_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {evlist = []  :: list(),
				sender_ip    :: inet:ip_address()
			   }).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get_events, SinceTime}=_Request, _From, State) ->
    Events = lists:takewhile(fun({_Type, _ElementId, TimeId}) -> TimeId >= SinceTime end, State#state.evlist),
	Reply = {ok, Events},
    {reply, Reply, State};
%% get/set the sender pid
handle_call(sender, _From, State) ->
	Reply = {ok, State#state.sender_ip},
	{reply, Reply, State};
handle_call({sender, NewSender}, _From, State) ->
	Reply = {ok, State#state{sender_ip=NewSender}},
	{reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({Type, ElementId}=_Msg, State) when (Type =:= click) or (Type =:= mouseenter) ->
	TimestampedMsg = {Type, ElementId, id()},
    {noreply, State#state{evlist=[TimestampedMsg | State#state.evlist]}}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

id() ->
	{Mega, Sec, Micro} = erlang:now(),
	Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
	integer_to_list(Id, 16).