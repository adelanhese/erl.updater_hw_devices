-module(updater_hw_devices_myplat1).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([read_fpga_reg_version/0]).

-export([power_cycle_myplat1_lc1/0]).
-export([get_slot_id_myplat1_lc1/0]).

-export([get_version_myplat1_lc1_fpgajic/0]).
-export([get_version_myplat1_fan_cpld/0]).
-export([get_version_myplat1_lc1_lpc55/0]).
-export([get_version_myplat1_fan_lpc55/0]).

-export([update_myplat1_lc1_fpgajic/0]).
-export([update_myplat1_lc1_fpgacvp/0]).
-export([update_myplat1_lc1_lpc55/0]).
-export([update_myplat1_fan_lpc55/0]).
-export([update_myplat1_fan_cpld/0]).




%-----------------------------------------------------------------------------
%
%                                T o D 0
% 
%-----------------------------------------------------------------------------

power_cycle_myplat1_lc1() ->
    {error, ?NOT_IMPLEMENTED}.


read_fpga_reg_version() ->
    {error, ?NOT_IMPLEMENTED}.

get_slot_id_myplat1_lc1() ->
    {error, ?NOT_IMPLEMENTED}.

get_version_myplat1_lc1_fpgajic() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_myplat1_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.

get_version_myplat1_lc1_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.

get_version_myplat1_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.

update_myplat1_lc1_fpgajic() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat1_lc1_fpgacvp() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat1_lc1_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat1_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat1_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.

