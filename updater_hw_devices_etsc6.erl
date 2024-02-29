-module(updater_hw_devices_etsc6).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([i2c_bkp_enable/0]).
-export([sc2000_cpld_rd/0]).
-export([sc2000_cpld_wr/0]).
-export([sc2000_jericho2c_turn_off/0]).

-export([get_slot_id_etsc6_mngt/0,
         get_slot_id_etsc6_lc4/0,
         get_slot_id_etsc6_sc2000/0,
         get_slot_id_etsc6_lc5/0]).

-export([power_cycle_etsc6_mngt/0,
         power_cycle_etsc6_lc4/0,
         power_cycle_etsc6_sc2000/0,
         power_cycle_etsc6_lc5/0]).

-export([get_version_etsc6_mngt_fpga/2,
         get_version_etsc6_fan_lpc55/2,
         get_version_etsc6_fan_cpld/2,
         get_version_etsc6_lc4_lpc55/2,
         get_version_etsc6_lc4_fpga/2,
         get_version_etsc6_lc4_cpld/2,
         get_version_etsc6_sc2000_lpc55/2,
         get_version_etsc6_sc2000_cpld/2,
         get_version_etsc6_lc5_lpc55/2,
         get_version_etsc6_lc5_fpga/2,
         get_version_etsc6_lc5_cpld/2,
         get_version_etsc6_lc5_pm6010/2]).

-export([update_etsc6_fan_lpc55/0,
         update_etsc6_fan_cpld/0,
         update_etsc6_mngt_fpga/0,
         update_etsc6_lc4_lpc55/0,
         update_etsc6_lc4_cpld/0,
         update_etsc6_lc4_fpga/0,
         update_etsc6_sc2000_lpc55/0,
         update_etsc6_sc2000_cpld/0,
         update_etsc6_lc5_cpld/0,
         update_etsc6_lc5_fpga/0,
         update_etsc6_lc5_pm6010/0]).

%-----------------------------------------------------------------------------
%
%                        T o D o
% 
%-----------------------------------------------------------------------------

i2c_bkp_enable() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_etsc6_mngt() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_etsc6_lc4() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_etsc6_sc2000() ->
    {error, ?NOT_IMPLEMENTED}.


sc2000_cpld_rd() ->
    {error, ?NOT_IMPLEMENTED}.


sc2000_cpld_wr() ->
    {error, ?NOT_IMPLEMENTED}.


sc2000_jericho2c_turn_off() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc6_mngt() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc6_lc4() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc6_sc2000() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_mngt_fpga(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "mngt", "fpga", "version", LocalPartNumber).

get_version_etsc6_fan_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "fan", "lpc55", "version", LocalPartNumber).

get_version_etsc6_fan_cpld(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "fan", "cpld", "version", LocalPartNumber).

get_version_etsc6_lc4_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc4", "lpc55", "version", LocalPartNumber).

get_version_etsc6_lc4_fpga(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc4", "fpga", "version", LocalPartNumber).

get_version_etsc6_lc4_cpld(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc4", "cpld", "version", LocalPartNumber).

get_version_etsc6_sc2000_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "sc2000", "lpc55", "version", LocalPartNumber).

get_version_etsc6_sc2000_cpld(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "sc2000", "cpld", "version", LocalPartNumber).

update_etsc6_fan_lpc55() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_fan_cpld() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


update_etsc6_mngt_fpga() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc4_lpc55() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc4_cpld() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


update_etsc6_sc2000_lpc55() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


update_etsc6_sc2000_cpld() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc4_fpga() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


get_slot_id_etsc6_lc5() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc6_lc5() ->
  io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
  {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc5_lpc55(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc5", "lpc55", "version", LocalPartNumber).

get_version_etsc6_lc5_fpga(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc5", "fpga", "version", LocalPartNumber).


get_version_etsc6_lc5_cpld(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc5", "cpld", "version", LocalPartNumber).


get_version_etsc6_lc5_pm6010(CfgFileName, LocalPartNumber) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, "lc5", "pm6010", "version", LocalPartNumber).

update_etsc6_lc5_cpld() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

update_etsc6_lc5_fpga() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

update_etsc6_lc5_pm6010() ->
    io:format("~p~n", [atom_to_list(?FUNCTION_NAME)]),
    {error, ?NOT_IMPLEMENTED}.

