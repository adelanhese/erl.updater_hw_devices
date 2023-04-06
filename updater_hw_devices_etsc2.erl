-module(updater_hw_devices_etsc2).
-include("updater_hw_devices_defines.hrl").

-export([get_version_etsc2/2]).
-export([update_etsc2/2]).


% ToDo
-export([get_slot_id_etsc2_mngt/0]).
-export([get_slot_id_etsc2_lc4/0]).

-export([power_cycle_etsc2_mngt/0]).
-export([power_cycle_etsc2_lc4/0]).
-export([power_cycle_etsc2_lc5/0]).

-export([get_version_etsc2_mngt_fpga/0]).
-export([get_version_etsc2_fan_lpc55/0]).
-export([get_version_etsc2_fan_cpld/0]).
-export([get_version_etsc2_lc4_lpc55/0]).
-export([get_version_etsc2_lc4_fpga/0]).
-export([get_version_etsc2_lc4_cpld/0]).
-export([get_version_etsc2_lc5_fpga/0]).
-export([get_version_etsc2_lc5_cpld/0]).
-export([get_version_etsc2_lc5_pm6010/0]).

-export([update_etsc2_fan_cpld/0]).
-export([update_etsc2_lc4_lpc55/0]).
-export([update_etsc2_lc4_fpga/0]).
-export([update_etsc2_lc5_lpc55/0]).
-export([update_etsc2_lc5_cpld/0]).
-export([update_etsc2_lc5_fpga/0]).
-export([update_etsc2_lc5_pm6010/0]).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_version_etsc2(string, string) -> {result, string}.
get_version_etsc2(Board, Device) -> 
    case Board of

        ?LC4 ->
            io:format("updater_hw_devices_etsc2:get_version_~p(~p)~n", [Board, Device]);

        ?LC5 ->
            io:format("updater_hw_devices_etsc2:get_version_~p(~p)~n", [Board, Device]);
    
        ?MNGT ->
            io:format("updater_hw_devices_etsc2:get_version_~p(~p)~n", [Board, Device]);

        ?FAN ->
            io:format("updater_hw_devices_etsc2:get_version_~p(~p)~n", [Board, Device]);

         _ ->
            error

    end,

    ok.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec update_etsc2(string, string) -> {result, string}.
update_etsc2(Board, Device) -> 
    case Board of

    ?LC4 ->
        io:format("updater_hw_devices_etsc2:update_~p(~p)~n", [Board, Device]);

    ?LC5 ->
        io:format("updater_hw_devices_etsc2:update_~p(~p)~n", [Board, Device]);

    ?MNGT ->
        io:format("updater_hw_devices_etsc2:update_~p(~p)~n", [Board, Device]);

    ?FAN ->
        io:format("updater_hw_devices_etsc2:update_~p(~p)~n", [Board, Device]);

    _ ->
        error

    end,

    ok.





%-----------------------------------------------------------------------------
%
%                           T o D o
% 
%-----------------------------------------------------------------------------

get_slot_id_etsc2_mngt() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_etsc2_lc4() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc2_mngt() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc2_lc4() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_mngt_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_lc4_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_lc4_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_lc4_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc4_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc4_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc2_lc5() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_lc5_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_lc5_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc2_lc5_pm6010() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc5_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc5_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc5_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc5_pm6010() ->
    {error, ?NOT_IMPLEMENTED}.

