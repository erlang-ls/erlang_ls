-module(els_telemetry).

-export([send_notification/1]).

-type event() :: map().

-spec send_notification(event()) -> ok.
send_notification(Event) ->
    Method = <<"telemetry/event">>,
    els_server:send_notification(Method, Event).
