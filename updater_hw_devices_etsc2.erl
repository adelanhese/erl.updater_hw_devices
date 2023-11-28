-module(updater_hw_devices_etsc2).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([get_slot_id_etsc2_mngt/0]).
-export([get_slot_id_etsc2_lc4/0]).

-export([power_cycle_etsc2_mngt/0,
         power_cycle_etsc2_lc4/0,
         power_cycle_etsc2_lc5/0]).

-export([get_version_etsc2_mngt_fpga/1,
         get_version_etsc2_fan_lpc55/1,
         get_version_etsc2_fan_cpld/1,
         get_version_etsc2_lc4_lpc55/1,
         get_version_etsc2_lc4_fpga/1,
         get_version_etsc2_lc4_cpld/1,
         get_version_etsc2_lc5_fpga/1,
         get_version_etsc2_lc5_cpld/1,
         get_version_etsc2_lc5_pm6010/1]).

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


get_version_etsc2_mngt_fpga(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "mngt", "fpga", "version").

get_version_etsc2_fan_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "fan", "lpc55", "version").

get_version_etsc2_fan_cpld(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "fan", "cpld", "version").

get_version_etsc2_lc4_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "lc4", "lpc55", "version").

get_version_etsc2_lc4_fpga(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "lc4", "fpga", "version").

get_version_etsc2_lc4_cpld(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "lc4", "cpld", "version").

get_version_etsc2_lc5_fpga(IniFile) ->
      % this is a stub. Must be replace with the correct code.
      updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "lc5", "fpga", "version").
  
get_version_etsc2_lc5_cpld(IniFile) ->
      % this is a stub. Must be replace with the correct code.
      updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "lc5", "cpld", "version").
  
get_version_etsc2_lc5_pm6010(IniFile) ->
      % this is a stub. Must be replace with the correct code.
      updater_hw_devices_cfgfileparse:read_field_from_cfg(IniFile, "lc5", "pm6010", "version").
  
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
