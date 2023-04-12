-module(updater_hw_devices_myplat6).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([i2c_bkp_enable/0]).
-export([sc2000_cpld_rd/0]).
-export([sc2000_cpld_wr/0]).
-export([sc2000_jericho2c_turn_off/0]).

-export([get_slot_id_myplat6_mngt/0,
         get_slot_id_myplat6_lc4/0,
         get_slot_id_myplat6_sc2000/0,
         get_slot_id_myplat6_lc5/0]).

-export([power_cycle_myplat6_mngt/0,
         power_cycle_myplat6_lc4/0,
         power_cycle_myplat6_sc2000/0,
         power_cycle_myplat6_lc5/0]).

-export([get_version_myplat6_mngt_fpga/1,
         get_version_myplat6_fan_lpc55/1,
         get_version_myplat6_fan_cpld/1,
         get_version_myplat6_lc4_lpc55/1,
         get_version_myplat6_lc4_fpga/1,
         get_version_myplat6_lc4_cpld/1,
         get_version_myplat6_sc2000_lpc55/1,
         get_version_myplat6_sc2000_cpld/1,
         get_version_myplat6_lc5_lpc55/1,
         get_version_myplat6_lc5_fpga/1,
         get_version_myplat6_lc5_cpld/1,
         get_version_myplat6_lc5_pm6010/1]).

-export([update_myplat6_fan_lpc55/0,
         update_myplat6_fan_cpld/0,
         update_myplat6_mngt_fpga/0,
         update_myplat6_lc4_lpc55/0,
         update_myplat6_lc4_cpld/0,
         update_myplat6_lc4_fpga/0,
         update_myplat6_sc2000_lpc55/0,
         update_myplat6_sc2000_cpld/0,
         update_myplat6_lc5_cpld/0,
         update_myplat6_lc5_fpga/0,
         update_myplat6_lc5_pm6010/0]).

%-----------------------------------------------------------------------------
%
%                        T o D o
% 
%-----------------------------------------------------------------------------

i2c_bkp_enable() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_myplat6_mngt() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_myplat6_lc4() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_myplat6_sc2000() ->
    {error, ?NOT_IMPLEMENTED}.


sc2000_cpld_rd() ->
    {error, ?NOT_IMPLEMENTED}.


sc2000_cpld_wr() ->
    {error, ?NOT_IMPLEMENTED}.


sc2000_jericho2c_turn_off() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_myplat6_mngt() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_myplat6_lc4() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_myplat6_sc2000() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_myplat6_mngt_fpga(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "mngt", "fpga", "version").

get_version_myplat6_fan_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "fan", "lpc55", "version").

get_version_myplat6_fan_cpld(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "fan", "cpld", "version").

get_version_myplat6_lc4_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc4", "lpc55", "version").

get_version_myplat6_lc4_fpga(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc4", "fpga", "version").

get_version_myplat6_lc4_cpld(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc4", "cpld", "version").

get_version_myplat6_sc2000_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "sc2000", "lpc55", "version").

get_version_myplat6_sc2000_cpld(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "sc2000", "cpld", "version").

update_myplat6_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_mngt_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_lc4_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_lc4_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_sc2000_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_sc2000_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_lc4_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_myplat6_lc5() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_myplat6_lc5() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_myplat6_lc5_lpc55(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc5", "lpc55", "version").

get_version_myplat6_lc5_fpga(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc5", "fpga", "version").


get_version_myplat6_lc5_cpld(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc5", "cpld", "version").


get_version_myplat6_lc5_pm6010(IniFile) ->
    % this is a stub. Must be replace with the correct code.
    updater_hw_devices_utils:read_field_from_cfg(IniFile, "lc5", "pm6010", "version").

update_myplat6_lc5_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_lc5_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


update_myplat6_lc5_pm6010() ->
    {error, ?NOT_IMPLEMENTED}.

