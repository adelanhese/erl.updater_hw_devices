-module(updater_hw_devices_etsc6).
-include("updater_hw_devices_defines.hrl").


% ToDo
-export([i2c_bkp_enable/0]).
-export([sc2000_cpld_rd/0]).
-export([sc2000_cpld_wr/0]).
-export([sc2000_jericho2c_turn_off/0]).

-export([get_slot_id_etsc6_mngt/0]).
-export([get_slot_id_etsc6_lc4/0]).
-export([get_slot_id_etsc6_sc2000/0]).
-export([get_slot_id_etsc6_lc5/0]).

-export([power_cycle_etsc6_mngt/0]).
-export([power_cycle_etsc6_lc4/0]).
-export([power_cycle_etsc6_sc2000/0]).
-export([power_cycle_etsc6_lc5/0]).

-export([get_version_etsc6_mngt_fpga/0]).
-export([get_version_etsc6_fan_lpc55/0]).
-export([get_version_etsc6_fan_cpld/0]).
-export([get_version_etsc6_lc4_lpc55/0]).
-export([get_version_etsc6_lc4_fpga/0]).
-export([get_version_etsc6_lc4_cpld/0]).
-export([get_version_etsc6_sc2000_lpc55/0]).
-export([get_version_etsc6_sc2000_cpld/0]).
-export([get_version_etsc6_lc5_lpc55/0]).
-export([get_version_etsc6_lc5_fpga/0]).
-export([get_version_etsc6_lc5_cpld/0]).
-export([get_version_etsc6_lc5_pm6010/0]).

-export([update_etsc6_fan_lpc55/0]).
-export([update_etsc6_fan_cpld/0]).
-export([update_etsc6_mngt_fpga/0]).
-export([update_etsc6_lc4_lpc55/0]).
-export([update_etsc6_lc4_cpld/0]).
-export([update_etsc6_sc2000_lpc55/0]).
-export([update_etsc6_sc2000_cpld/0]).
-export([update_etsc6_lc4_fpga/0]).
-export([update_etsc6_lc5_cpld/0]).
-export([update_etsc6_lc5_fpga/0]).
-export([update_etsc6_lc5_pm6010/0]).
    



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


get_version_etsc6_mngt_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc4_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc4_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc4_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_sc2000_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_sc2000_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_fan_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_fan_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_mngt_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc4_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc4_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_sc2000_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_sc2000_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc4_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_slot_id_etsc6_lc5() ->
    {error, ?NOT_IMPLEMENTED}.


power_cycle_etsc6_lc5() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc5_lpc55() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc5_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc5_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


get_version_etsc6_lc5_pm6010() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc5_cpld() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc5_fpga() ->
    {error, ?NOT_IMPLEMENTED}.


update_etsc6_lc5_pm6010() ->
    {error, ?NOT_IMPLEMENTED}.

