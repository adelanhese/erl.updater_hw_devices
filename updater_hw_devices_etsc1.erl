-module(updater_hw_devices_etsc1).
-include("updater_hw_devices_defines.hrl").

-export([get_version_etsc1/2]).
-export([update_etsc1/2]).

% ToDo

-export([read_fpga_reg_version/0]).

-export([power_cycle_etsc1_lc1/0]).
-export([get_slot_id_etsc1_lc1/0]).

-export([get_version_etsc1_lc1_fpgajic/0]).
-export([get_version_etsc1_fan_cpld/0]).
-export([get_version_etsc1_lc1_lpc55/0]).
-export([get_version_etsc1_fan_lpc55/0]).

-export([update_etsc1_lc1_fpgajic/0]).
-export([update_etsc1_lc1_fpgacvp/0]).
-export([update_etsc1_lc1_lpc55/0]).
-export([update_etsc1_fan_lpc55/0]).
-export([update_etsc1_fan_cpld/0]).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_version_etsc1(string, string) -> {result, string}.
get_version_etsc1(Board, Device) -> 
    case Board of

        ?LC1 ->
            io:format("updater_hw_devices_etsc1:get_version_~p(~p)~n", [Board, Device]);

        ?FAN ->
            io:format("updater_hw_devices_etsc1:get_version_~p(~p)~n", [Board, Device]);

         _ ->
            error

    end,

    ok.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec update_etsc1(string, string) -> {result, string}.
update_etsc1(Board, Device) -> 
    case Board of

        ?LC1 ->
            io:format("updater_hw_devices_etsc1:update_~p(~p)~n", [Board, Device]);

        ?FAN ->
            io:format("updater_hw_devices_etsc1:update_~p(~p)~n", [Board, Device]);

         _ ->
            error

    end,

    ok.







%-----------------------------------------------------------------------------
%
%                                T o D 0
% 
%-----------------------------------------------------------------------------

power_cycle_etsc1_lc1() ->
    {error, ?NOT_IMPLEMENTED}.


read_fpga_reg_version() ->
    {error, ?NOT_IMPLEMENTED}.

get_slot_id_etsc1_lc1() ->
    {error, ?NOT_IMPLEMENTED}.

get_version_etsc1_lc1_fpgajic() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc1_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.

get_version_etsc1_lc1_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.

get_version_etsc1_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.

update_etsc1_lc1_fpgajic() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc1_lc1_fpgacvp() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc1_lc1_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc1_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc1_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.

