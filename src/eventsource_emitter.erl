-module(eventsource_emitter).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
	timer:send_after(1000, {event, <<"sender_you">>}),
	timer:send_interval(1000, send),
	timer:send_after(10000, shutdown),
	{ok, Req, undefined}.

handle(Req, State) ->
	Headers = [{'Content-Type', <<"text/event-stream">>}],
	{ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
	handle_loop(Req2, State).

handle_loop(Req, State) ->
	receive
		shutdown ->
			{ok, Req, State};
		
		%% informational messages
		{event, Message} ->
			Event = ["id: ", id(), "\ndata: ", Message, "\n\n"],
			ok = cowboy_http_req:chunk(Event, Req),
			handle_loop(Req, State);
		
		%% sending the list of events. we care about this.
		send ->
			case cowboy_http_req:header('Last-Event-ID', Req) of
				{undefined, Req} -> LastEventId = 0;
				{Value, Req} -> LastEventId = list_to_integer(binary_to_list(Value))
			end,
			{ok, Events} = gen_server:call(event_history_server, {get_events, LastEventId}),
			emit_events(Events, Req),
			handle_loop(Req, State)
	end.

terminate(_Req, _State) ->
	ok.


%% INTERNAL

emit_events([], Req) ->
	ok;
emit_events([{Type, ElementId, TimeId}=_Event | Events], Req) ->
	Response = ["id: ", TimeId, "\ndata: ", Type, ":", ElementId, "\n\n"],
	ok = cowboy_http_req:chunk(Response, Req).

id() ->
	{Mega, Sec, Micro} = erlang:now(),
	Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
	integer_to_list(Id, 16).