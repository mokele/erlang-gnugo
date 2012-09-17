-module(gnugo).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    move/3,
    genmove/2, genmove/3,
    boardsize/2, clear/1
  ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
  ]).

-record(s, {
    seq :: pos_integer(),
    port :: port(),
    size :: 9 | 13 | 19,
    waiting_for_replies :: ets:tid()
  }).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?MODULE, [], []).

move(Pid, Color, At) ->
  gen_server:call(Pid, {move, Color, At}).

genmove(Pid, For) ->
  gen_server:call(Pid, {genmove, For}).

genmove(Pid, For, Timeout) ->
  gen_server:call(Pid, {genmove, For}, Timeout).

boardsize(Pid, Size) ->
  gen_server:call(Pid, {boardsize, Size}).

clear(Pid) ->
  gen_server:call(Pid, clear).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  Command = "gnugo --mode gtp",
  Port = open_port({spawn, Command}, [
      stream, binary, {line, 50}
    ]),
  State = #s{
    seq = 1,
    port = Port,
    waiting_for_replies = ets:new(?MODULE, [set])
  },
  {ok, State}.

handle_call({boardsize, Size}, From, State0) ->
  Handler =
    fun(_, HandlerState) ->
        gen_server:reply(From, ok),
        HandlerState
    end,
  State = command(["boardsize ", integer_to_list(Size)], Handler, State0),
  {noreply, State};
handle_call(clear, From, State0) ->
  Handler =
    fun(_, HandlerState) ->
        gen_server:reply(From, ok),
        HandlerState
    end,
  State = command("clear_board", Handler, State0),
  {noreply, State};
handle_call({genmove, For}, From, State0) ->
  Handler =
    fun(Arg, HandlerState) ->
        Move =
          case Arg of
            "PASS" -> pass;
            "pass" -> pass;
            "RESIGN" -> resign;
            "resign" -> resign;
            _ -> Arg
          end,
        gen_server:reply(From, {ok, Move}),
        HandlerState
    end,
  State = command(["genmove ", string:to_lower(atom_to_list(For))], Handler, State0),
  {noreply, State};

handle_call({move, For, At0}, From, #s{
    waiting_for_replies = _Waiting
  } = State0) ->
  Handler =
    fun(_, HandlerState) ->
        gen_server:reply(From, ok),
        HandlerState
    end,
  At =
    case At0 of
      pass -> "PASS";
      _ -> At0
    end,

  State = command(["play ", string:to_lower(atom_to_list(For)), " ", At], Handler, State0),
  {noreply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({Port, {data, {eol,<<>>}}}, #s{port = Port} = State) ->
  {noreply, State};
handle_info({Port, {data, Data}}, #s{port = Port} = State0) ->
  State = handle_data(Data, State0),
  {noreply, State};
handle_info({Port, {exit_status, _Status}}, #s{port = Port} = State) ->
  {stop, port_died, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_data({eol, <<$=,R/binary>>}, State) ->
  case re:run(R, "^([0-9]+) (.*)$", [{capture, all, list}]) of
    {match, [_, SeqL, Args]} ->
      reply(list_to_integer(SeqL), Args, State);
    nomatch ->
      State
  end;
handle_data(_, State) ->
  State.

reply(Seq, Args, #s{waiting_for_replies = Waiting} = State) ->
  case ets:lookup(Waiting, Seq) of
    [{Seq,Handler}] ->
      ets:delete(Waiting, Seq),
      Handler(Args, State);
    [] ->
      State
  end.

command(Command, Handler, #s{
    seq = Seq,
    port = Port,
    waiting_for_replies = Waiting
  } = State) ->
  Line = [integer_to_list(Seq), " ", Command, "\n"],
  Port ! {self(), {command, iolist_to_binary(Line)}},
  ets:insert_new(Waiting, {Seq, Handler}),
  State#s{
    seq = Seq + 1
  }.

-ifdef(TEST).

go_test_() ->
  {timeout, 120,
    fun() ->
        {ok, Pid} = start_link(),
        boardsize(Pid, 9),
        clear(Pid),
        ok = move(Pid, 'WHITE', "D5"),
        {ok, _Move} = genmove(Pid, 'BLACK'),
        lists:foldl(
          fun(N, Stone) ->
              {ok, Move} = genmove(Pid, Stone),
              ?debugFmt("~p ~p", [N, Move]),
              opposite(Stone)
          end,
          'WHITE',
          lists:seq(1, 100)
        ),
        ok
    end
  }.

opposite('BLACK') -> 'WHITE';
opposite('WHITE') -> 'BLACK'.

-endif.
