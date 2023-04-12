-module(updater_hw_devices_plat1).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([read_fpga_reg_version/0]).

-export([power_cycle_plat1_lc1/0]).
-export([get_slot_id_plat1_lc1/0]).

-export([get_version_plat1_lc1_fpgajic/1,
         get_version_plat1_fan_cpld/1,
         get_version_plat1_lc1_lpc55/1,
         get_version_plat1_fan_lpc55/1]).

-export([update_plat1_lc1_fpgajic/0,
         update_plat1_lc1_fpgacvp/0,
         update_plat1_lc1_lpc55/0,
         update_plat1_fan_lpc55/0,
         update_plat1_fan_cpld/0]).

%-----------------------------------------------------------------------------
%
%                                T o D 0
% 
%-----------------------------------------------------------------------------

power_cycle_plat1_lc1() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


read_fpga_reg_version() ->
    {error, ?NOT_IMPLEMENTED}.

get_slot_id_plat1_lc1() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


get_version_plat1_lc1_fpgajic(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc1", "fpgajic", "version").


get_version_plat1_fan_cpld(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "fan", "cpld", "version").

get_version_plat1_lc1_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc1", "lpc55", "version").

get_version_plat1_fan_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "fan", "lpc55", "version").

update_plat1_lc1_fpgajic() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_plat1_lc1_fpgacvp() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_plat1_lc1_lpc55() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_plat1_fan_lpc55() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_plat1_fan_cpld() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

