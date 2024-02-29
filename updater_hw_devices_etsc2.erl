-module(updater_hw_devices_etsc2).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([get_slot_id_etsc2_mngt/0]).
-export([get_slot_id_etsc2_lc4/0]).

-export([power_cycle_etsc2_mngt/0,
         power_cycle_etsc2_lc4/0,
         power_cycle_etsc2_lc5/0]).

-export([get_version_etsc2_mngt_fpga/2,
         get_version_etsc2_fan_lpc55/2,
         get_version_etsc2_fan_cpld/2,
         get_version_etsc2_lc4_lpc55/2,
         get_version_etsc2_lc4_fpga/2,
         get_version_etsc2_lc4_cpld/2,
         get_version_etsc2_lc5_fpga/2,
         get_version_etsc2_lc5_cpld/2,
         get_version_etsc2_lc5_pm6010/2]).

-export([update_etsc2_fan_cpld/0,
         update_etsc2_lc4_lpc55/0,
         update_etsc2_lc4_fpga/0,
         update_etsc2_lc5_lpc55/0,
         update_etsc2_lc5_cpld/0,
         update_etsc2_lc5_fpga/0,
         update_etsc2_lc5_pm6010/0]).

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


get_version_etsc2_mngt_fpga(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "mngt", "fpga", "version", LocalPartNumber).

get_version_etsc2_fan_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "fan", "lpc55", "version", LocalPartNumber).

get_version_etsc2_fan_cpld(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "fan", "cpld", "version", LocalPartNumber).

get_version_etsc2_lc4_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc4", "lpc55", "version", LocalPartNumber).

get_version_etsc2_lc4_fpga(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc4", "fpga", "version", LocalPartNumber).

get_version_etsc2_lc4_cpld(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc4", "cpld", "version", LocalPartNumber).

get_version_etsc2_lc5_fpga(CfgFileName, LocalPartNumber) ->
      % this is a stub. Must be replace with the correct code.
      updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc5", "fpga", "version", LocalPartNumber).
  
get_version_etsc2_lc5_cpld(CfgFileName, LocalPartNumber) ->
      % this is a stub. Must be replace with the correct code.
      updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc5", "cpld", "version", LocalPartNumber).
  
get_version_etsc2_lc5_pm6010(CfgFileName, LocalPartNumber) ->
      % this is a stub. Must be replace with the correct code.
      updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc5", "pm6010", "version", LocalPartNumber).
  
update_etsc2_fan_cpld() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc4_lpc55() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc4_fpga() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc2_lc5() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

update_etsc2_lc5_lpc55() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc5_cpld() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc5_fpga() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_etsc2_lc5_pm6010() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

