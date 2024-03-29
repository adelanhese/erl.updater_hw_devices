-module(updater_hw_devices_etsc1).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([read_fpga_reg_version/0]).

-export([power_cycle_etsc1_lc1/0]).
-export([get_slot_id_etsc1_lc1/0]).

-export([get_version_etsc1_lc1_fpgajic/2,
         get_version_etsc1_fan_cpld/2,
         get_version_etsc1_lc1_lpc55/2,
         get_version_etsc1_lc1_cy7c65211a/2,
         get_version_etsc1_fan_lpc55/2]).

-export([update_etsc1_lc1_fpgajic/0,
         update_etsc1_lc1_fpgacvp/0,
         update_etsc1_lc1_lpc55/0,
         update_etsc1_lc1_cy7c65211a/0,
         update_etsc1_fan_lpc55/0,
         update_etsc1_fan_cpld/0]).

%-----------------------------------------------------------------------------
%
%                                T o D 0
% 
%-----------------------------------------------------------------------------

power_cycle_etsc1_lc1() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

read_fpga_reg_version() ->
    {error, ?NOT_IMPLEMENTED}.

get_slot_id_etsc1_lc1() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

get_version_etsc1_lc1_fpgajic(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc1", "fpgajic", "version", LocalPartNumber).

get_version_etsc1_fan_cpld(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "fan", "cpld", "version", LocalPartNumber).

get_version_etsc1_lc1_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc1", "lpc55", "version", LocalPartNumber).

get_version_etsc1_lc1_cy7c65211a(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc1", "cy7c65211a", "version", LocalPartNumber).

get_version_etsc1_fan_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "fan", "lpc55", "version", LocalPartNumber).

update_etsc1_lc1_fpgajic() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

update_etsc1_lc1_fpgacvp() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

update_etsc1_lc1_lpc55() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

update_etsc1_lc1_cy7c65211a() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

update_etsc1_fan_lpc55() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.
  
update_etsc1_fan_cpld() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

