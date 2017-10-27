-module(ical2org).
-export([start/1, start/0]).

fmt_fields([]) -> [];
fmt_fields([{Fmt, Fields} | Rest]) ->
    case Fields of
	[[]] -> "";
	_ -> io_lib:format(Fmt, Fields)
    end ++ fmt_fields(Rest).

adjust_time({Time, TZ}) ->
    NTZ = case string:to_integer(TZ) of
	      {error, _} -> 0; % non-numeric, let's assume UTC
	      {N, _} -> N
	  end,
    % FIXME support non-hour-aligned offsets
    case Time div 100 - NTZ of
	A when A >= 2400 -> {1, A - 2400};
	A when A < 0 -> {-1, A + 2400};
	A -> {0, A}
    end.

leap_day(Y) ->
    if Y rem 4 =/= 0 -> 0;
       Y rem 100 =/= 0 -> 1;
       Y rem 400 =/= 0 -> 0;
       true -> 1
    end.

month_days(Y, M) when Y > 0, M > 0, M < 13 ->
    if M =:= 2 -> 28 + leap_day(Y);
       M < 8 -> 30 + (M rem 2);
       true -> 31 - (M rem 2)
    end.

adjust_date(Y, 1, 0) ->
    {Y - 1, 12, 31};
adjust_date(Y, M, 0) when M > 1 ->
    adjust_date(Y, M - 1, month_days(Y, M - 1));
adjust_date(Y, M, _) when M > 12 ->
    adjust_date(Y + 1, M - 12, 1);
adjust_date(Y, M, D) when M > 0, D > 0 ->
    MaxD = month_days(Y, M),
    if D > MaxD -> adjust_date(Y, M + 1, 1);
       true -> {Y, M, D}
    end.

adjust_date(D) ->
    adjust_date(D div 10000, D div 100 rem 100, D rem 100).

fmt_date([]) -> [];
fmt_date([H | T]) when is_list(H) ->
    [fmt_date(H) | fmt_date(T)];
% XXX: org-mode doesn't support time zones, we return UTC
fmt_date(DateStr) ->
    Date = case string:to_integer(DateStr) of
	       {RawDate, [$T | TimeStr]} ->
		   {DateAdjustment, Time} =
		       adjust_time(string:to_integer(TimeStr)),
		   RawDate + DateAdjustment;
	       {RawDate, _} ->
		   Time = -1,
		   RawDate
	   end,
    {Y, M, D} = adjust_date(Date),
    Fmt = "~4..0w-~2..0w-~2..0w" ++ if Time < 0 -> "~i~i";
				       true -> " ~2..0w:~2..0w"
				    end,
    HH = Time div 100,
    MM = Time rem 100,
    io_lib:format(Fmt, [Y, M, D, HH, MM]).

parse_line(Str) when is_list(Str) ->
    [FirstLine | Rest] = string:split(lists:droplast(Str), "\\", all),
    FirstLine ++
	lists:flatten(
	  lists:map(fun ([$n | []]) -> [10]; % don't indent empty lines
			([$n | X]) -> [10, 32, 32, 32 | X];
			(X = [C | _]) when C =:= $, -> X;
			(X) -> [$\ | X]
		    end, Rest));
parse_line(X) -> X.

read_line(File) ->
    read_line(File, "").
read_line(File, Line) ->
    case Line of
	eof -> eof;
	_ ->
	    case {Line, parse_line(io:get_line(File, ""))} of
		{_, {error, X}} -> {error, X};
		{_, [32 | A]} -> read_line(File, Line ++ A);
		{"", NextLine} -> read_line(File, NextLine);
		{_, NextLine} ->
		    [NameNParams, Value] = string:split(Line, ":"),
		    [Name | Params] = string:split(NameNParams, ";"),
		    {Name, Params, Value,
		     fun() -> read_line(File, NextLine) end}
	    end
    end.


unimplemented(C) ->
    io:fwrite(standard_error, "Unimplemented component: ~s~n", [C]),
    "".

prop_get([], _) -> [];
prop_get([K | T], List) ->
    Values = lists:map(fun({_, V}) -> V end, proplists:get_all_values(K, List)),
    [lists:flatten(lists:join(10, Values)) | prop_get(T, List)].

fmt_component("VCALENDAR", Props, Children) ->
    [Name] = prop_get(["X-WR-CALNAME"], Props),
    io_lib:format("* Calendar: ~s~n~s", [Name, lists:flatten(Children)]);
fmt_component("VEVENT", Props, Children) ->
    Res = fmt_fields([{"** ~s ~s~n", prop_get(["STATUS", "SUMMARY"], Props)},
		{"   <~s>--<~s>~n", fmt_date(prop_get(["DTSTART", "DTEND"], Props))},
		{"   Timestamp: [~s]~n", fmt_date(prop_get(["DTSTAMP"], Props))},
		{"   Created: [~s]~n", fmt_date(prop_get(["CREATED"], Props))},
		{"   Last Modified: [~s]~n", fmt_date(prop_get(["LAST-MODIFIED"], Props))},
		{"   Organizer: ~s~n", prop_get(["ORGANIZER"], Props)},
		{"   Attendees: ~s~n", prop_get(["ATTENDEE"], Props)},
		{"   Google Hangouts URL: ~s~n", prop_get(["X-GOOGLE-HANGOUT"], Props)},
		{"   Location: ~s~n", prop_get(["LOCATION"], Props)},
		{"~n   ~s~n", prop_get(["DESCRIPTION"], Props)}]),
    Res ++ Children;
fmt_component(C = "VTODO", _Props, _) ->
    unimplemented(C);
fmt_component(C = "VTIMEZONE", _Props, _) ->
    unimplemented(C);
fmt_component(C = "VALARM", _Props, _) ->
    unimplemented(C);
fmt_component(C = "VFREEBUSY", _Props, _) ->
    unimplemented(C);
fmt_component(C = "VJOURNAL", _Props, _) ->
    unimplemented(C).

parse(_, _, _, _, eof) -> "";
%% BEGIN:<Component>
parse(File, CompS, PropS, ChildS, {"BEGIN", [], Component, NextLine}) ->
    NewCompS = [Component | CompS],
    NewPropS = [[] | PropS],
    NewChildS = [[] | ChildS],
    parse(File, NewCompS, NewPropS, NewChildS, NextLine());
%% Last END:<Component> in a calendar
parse(File, [Component | []], [Props | _], [Children | _],
      {"END", [], Component, NextLine}) ->
    fmt_component(Component, Props, Children) ++
	% Next calendar
	parse(File, [], [], [], NextLine());
% END:<Component>
parse(File, [Component | CompS], [Props | PropS], [Children, Siblings | ChildS],
      {"END", [], Component, NextLine}) ->
    NewChld = Siblings ++ fmt_component(Component, Props, Children),
    parse(File, CompS, PropS, [NewChld | ChildS], NextLine());
% END:<Component>, unmatched
parse(File, CompS, PropS, ChildS, {"END", [], Component, NextLine}) ->
    [Ctx | _] = CompS,
    io:fwrite(standard_error,
	      "Warning: Unmatched END tag for ~s, in ~s context.~n",
	      [Component, Ctx]),
    parse(File, CompS, PropS, ChildS, NextLine());
% <Property>[;Params...]:<Value>
parse(File, CompS, [CurProps | PropS], ChildS,
      {Prop, Params, Val, NextLine}) ->
    NewPropS = [[{Prop, {Params, Val}} | CurProps] | PropS],
    parse(File, CompS, NewPropS, ChildS, NextLine()).

parse(File) ->
    parse(File, [], [], "", read_line(File)).

main(Output, Input) ->
    io:fwrite(Output, "~s~n", [parse(Input)]),
    halt().


start() ->
    ok.

start([Input]) ->
    {ok, InFile} = file:open(Input, [read]),
    main(standard_io, InFile);

start([Input, Output]) ->
    {ok, InFile} = file:open(Input, [read]),
    {ok, OutFile} = file:open(Output, [write]),
    main(OutFile, InFile).
