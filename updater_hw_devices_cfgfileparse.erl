-module(updater_hw_devices_cfgfileparse).
-include("updater_hw_devices_defines.hrl").

% Already tested and working
-export([read_field_from_cfg_new/5,
         read_field_from_cfg/4,

         read_field_from_cfg_anyway_new/5,
         read_field_from_cfg_anyway/4,

         get_device_index_from_cfg_new/6,
         get_device_index_from_cfg/5,

         get_file_image_name/4,

         check_for_supported_devices_new/6,
         check_for_supported_devices/5,
         
         enable_disable_device_new/6,
         enable_disable_device/5,
         
         show_devices_new/4,
         show_devices/3,

         show_boards_tree_new/4,
         show_boards_tree/3,

         read_cfg_file/3,
         write_cfg_file/4,

         check_for_supported_board/2,
         board_2ek_match/3,
         update_cfg_file/2]).

         
%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_cfg_file(string, string, string) -> {error|ok, string}.
read_cfg_file(CfgFile, Sector, FieldIndex) ->
    Xml = updater_hw_devices_utils:substring_exists(".xml", CfgFile),
    Ini = updater_hw_devices_utils:substring_exists(".cfg", CfgFile),
    read_cfg_file(CfgFile, Sector, FieldIndex, Xml, Ini).
read_cfg_file(CfgFile, Sector, FieldIndex, true, _Ini) ->
    updater_hw_devices_xmlfilesupport:xml_rd_file(CfgFile, Sector, FieldIndex);
read_cfg_file(CfgFile, Sector, FieldIndex, _Xml, true) ->
    updater_hw_devices_inifilesupport:ini_file(CfgFile, Sector, FieldIndex);
read_cfg_file(_CfgFile, _Sector, _FieldIndex, _Xml, _Ini) ->
    {error, "Invalid cfg file type"}. 

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec write_cfg_file(string, string, string, sring) -> ok | error.
write_cfg_file(CfgFile, Sector, FieldIndex, NewFieldValue) ->
  updater_hw_devices_xmlfilesupport:xml_wr_file(CfgFile, Sector, FieldIndex, NewFieldValue).


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec board_2ek_match(string, string, string) -> {result}.
board_2ek_match(_Chrono2ek, VfVrIcs2ek, _LocalPartNumber) when (VfVrIcs2ek == "") ->
    match;
board_2ek_match(Chrono2ek, VfVrIcs2ek, LocalPartNumber) ->
    {Result1, LocalChrono2ek, LocalVfVrIcs2ek} = updater_hw_devices_utils:split_partnumber(LocalPartNumber),
    Match_VfVrIcs2ek = updater_hw_devices_utils:substring_exists(LocalVfVrIcs2ek, VfVrIcs2ek),
    board_2ek_check_match(Result1, Match_VfVrIcs2ek, Chrono2ek, LocalChrono2ek).

board_2ek_check_match(ok, true, Chrono2ek, LocalChrono2ek) when (Chrono2ek == LocalChrono2ek) ->
    match;
board_2ek_check_match(_Result1, _Match_VfVrIcs2ek, _Chrono2ek, _LocalChrono2ek) ->
    nomatch.


%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec read_field_from_cfg_new(string, string, string, string, string) -> {result, string}.
read_field_from_cfg_new(CfgFileName, Board, Device, Field, LocalPartNumber) ->
    {Result1, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {Result2, Chrono2ekStr} = read_cfg_file(CfgFileName, Board, "chrono_2ek"),
    read_field_from_cfg_new({Result1, NumDevicesStr}, {Result2, Chrono2ekStr}, CfgFileName, Board, Device, Field, LocalPartNumber).

read_field_from_cfg_new({ok, NumDevicesStr}, {ok, Chrono2ekStr}, CfgFileName, Board, Device, Field, LocalPartNumber) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    read_field_from_cfg_search_for_device_new(CfgFileName, Board, Device, Field, 1, NumDevices, Chrono2ekStr, LocalPartNumber);
read_field_from_cfg_new({_Result1, NumDevicesStr}, {_Result2, _Chrono2ekStr}, _IniFile, _Board, _Device, _Field, _LocalPartNumber) ->
    {error, NumDevicesStr}.

read_field_from_cfg_search_for_device_new(_IniFile, _Board, _Device, _Field, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    {error, "field not found"};
read_field_from_cfg_search_for_device_new(CfgFileName, Board, Device, Field, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    {Result1, DevicesStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, EnabledStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    {Result3, VfVrIcs2ekStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    read_field_from_cfg_search_for_device_new({Result1, DevicesStr}, {Result2, EnabledStr}, {Result3, Board2ekMatch}, CfgFileName, Board, Device, Field, Index, MaxDevices, Chrono2ekStr, LocalPartNumber).

read_field_from_cfg_search_for_device_new({ok, DevicesStr}, {ok, "1"}, {ok, match}, CfgFileName, Board, Device, Field, Index, _MaxDevices, _Chrono2ekStr, _LocalPartNumber) when
    (DevicesStr == Device) ->
        read_cfg_file(CfgFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));
read_field_from_cfg_search_for_device_new({_Result1, _DevicesStr}, {_Result2, _EnabledStr}, {_Result3, _Board2ekMatch}, CfgFileName, Board, Device, Field, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    read_field_from_cfg_search_for_device_new(CfgFileName, Board, Device, Field, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber).

%%-----------------------------------------------------------------------------
%%
%% ToDo: to include 2ek
%%-----------------------------------------------------------------------------
-spec read_field_from_cfg(string, string, string, string) -> {result, string}.
read_field_from_cfg(CfgFileName, Board, Device, Field) ->
    {Result, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    read_field_from_cfg({Result, NumDevicesStr}, CfgFileName, Board, Device, Field).

read_field_from_cfg({ok, NumDevicesStr}, CfgFileName, Board, Device, Field) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    read_field_from_cfg_search_for_device(CfgFileName, Board, Device, Field, 1, NumDevices);
read_field_from_cfg({_Result, NumDevicesStr}, _IniFile, _Board, _Device, _Field) ->
    {error, NumDevicesStr}.

read_field_from_cfg_search_for_device(_IniFile, _Board, _Device, _Field, Index, MaxDevices) when (Index > MaxDevices) ->
    {error, "field not found"};
read_field_from_cfg_search_for_device(CfgFileName, Board, Device, Field, Index, MaxDevices) ->
    {Result1, DevicesStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, EnabledStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    read_field_from_cfg_search_for_device({Result1, DevicesStr}, {Result2, EnabledStr}, CfgFileName, Board, Device, Field, Index, MaxDevices).

read_field_from_cfg_search_for_device({ok, DevicesStr}, {ok, "1"}, CfgFileName, Board, Device, Field, Index, _MaxDevices) when
    (DevicesStr == Device) ->
        read_cfg_file(CfgFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));
read_field_from_cfg_search_for_device({_Result1, _DevicesStr}, {_Result2, _EnabledStr}, CfgFileName, Board, Device, Field, Index, MaxDevices) ->
    read_field_from_cfg_search_for_device(CfgFileName, Board, Device, Field, Index + 1, MaxDevices).

%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec read_field_from_cfg_anyway_new(string, string, string, string, string) -> {result, string}.
read_field_from_cfg_anyway_new(CfgFileName, Board, Device, Field, LocalPartNumber) ->
    {Result1, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {Result2, Chrono2ekStr} = read_cfg_file(CfgFileName, Board, "chrono_2ek"),
    read_field_from_cfg_anyway_new({Result1, NumDevicesStr}, {Result2, Chrono2ekStr}, CfgFileName, Board, Device, Field, LocalPartNumber).

read_field_from_cfg_anyway_new({ok, NumDevicesStr}, {ok, Chrono2ekStr}, CfgFileName, Board, Device, Field, LocalPartNumber) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    read_field_from_cfg_anyway_search_for_device_new(CfgFileName, Board, Device, Field, 1, NumDevices, Chrono2ekStr, LocalPartNumber);
read_field_from_cfg_anyway_new({_Result1, NumDevicesStr},  {_Result2, _Chrono2ekStr}, _IniFile, _Board, _Device, _Field, _LocalPartNumber) ->
    {error, NumDevicesStr}.

read_field_from_cfg_anyway_search_for_device_new(_IniFile, _Board, _Device, _Field, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    {error, "field not found"};
read_field_from_cfg_anyway_search_for_device_new(CfgFileName, Board, Device, Field, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    {Result1, DevicesStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, VfVrIcs2ekStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    read_field_from_cfg_anyway_search_for_device_new({Result1, DevicesStr}, {Result2, Board2ekMatch}, CfgFileName, Board, Device, Field, Index, MaxDevices, Chrono2ekStr, LocalPartNumber).

read_field_from_cfg_anyway_search_for_device_new({ok, DevicesStr}, {ok, match}, CfgFileName, Board, Device, Field, Index, _MaxDevices, _Chrono2ekStr, _LocalPartNumber) when
    (DevicesStr == Device) ->
           read_cfg_file(CfgFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));
read_field_from_cfg_anyway_search_for_device_new({_Result1, _DevicesStr}, {_Result2, _Board2ekMatch}, CfgFileName, Board, Device, Field, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    read_field_from_cfg_anyway_search_for_device_new(CfgFileName, Board, Device, Field, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber).


%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec read_field_from_cfg_anyway(string, string, string, string) -> {result, string}.
read_field_from_cfg_anyway(CfgFileName, Board, Device, Field) ->
    {Result, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    read_field_from_cfg_anyway({Result, NumDevicesStr}, CfgFileName, Board, Device, Field).

read_field_from_cfg_anyway({ok, NumDevicesStr}, CfgFileName, Board, Device, Field) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    read_field_from_cfg_anyway_search_for_device(CfgFileName, Board, Device, Field, 1, NumDevices);
read_field_from_cfg_anyway({_Result, NumDevicesStr}, _IniFile, _Board, _Device, _Field) ->
    {error, NumDevicesStr}.

read_field_from_cfg_anyway_search_for_device(_IniFile, _Board, _Device, _Field, Index, MaxDevices) when (Index > MaxDevices) ->
    {error, "field not found"};
read_field_from_cfg_anyway_search_for_device(CfgFileName, Board, Device, Field, Index, MaxDevices) ->
    {Result1, DevicesStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    read_field_from_cfg_anyway_search_for_device({Result1, DevicesStr}, CfgFileName, Board, Device, Field, Index, MaxDevices).

read_field_from_cfg_anyway_search_for_device({ok, DevicesStr}, CfgFileName, Board, Device, Field, Index, _MaxDevices) when
    (DevicesStr == Device) ->
           read_cfg_file(CfgFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));
read_field_from_cfg_anyway_search_for_device({_Result1, _DevicesStr}, CfgFileName, Board, Device, Field, Index, MaxDevices) ->
    read_field_from_cfg_anyway_search_for_device(CfgFileName, Board, Device, Field, Index + 1, MaxDevices).


%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec get_device_index_from_cfg_new(string, string, string, string, string, string) -> {result, number}.
get_device_index_from_cfg_new(IniFileName, Board, Device, Field, Value, LocalPartNumber) ->
    {Result1, NumDevicesStr} = read_cfg_file(IniFileName, Board, "num_devices"),
    {Result2, Chrono2ekStr} = read_cfg_file(IniFileName, Board, "chrono_2ek"),
    get_device_index_from_cfg_new({Result1, NumDevicesStr}, {Result2, Chrono2ekStr}, IniFileName, Board, Device, Field, Value, LocalPartNumber).

get_device_index_from_cfg_new({ok, NumDevicesStr}, {ok, Chrono2ekStr}, IniFileName, Board, Device, Field, Value, LocalPartNumber) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    get_device_index_from_cfg_search_for_device_new(IniFileName, Board, Device, Field, Value, 1, NumDevices, Chrono2ekStr, LocalPartNumber);
get_device_index_from_cfg_new({_Result, _NumDevicesStr}, {_Result2, _Chrono2ekStr}, _IniFileName, _Board, _Device, _Field, _Value, _LocalPartNumber) ->
    {error, -1}.

get_device_index_from_cfg_search_for_device_new(_IniFileName, _Board, _Device, _Field, _Value, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    {error, -1};
get_device_index_from_cfg_search_for_device_new(IniFileName, Board, Device, Field, Value, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    {Result1, DevicesValueStr} = read_cfg_file(IniFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, FieldValueStr} = read_cfg_file(IniFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)])),
    {Result3, VfVrIcs2ekStr} = read_cfg_file(IniFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    get_device_index_from_cfg_search_for_device_new({Result1, DevicesValueStr}, {Result2, FieldValueStr},  {Result3, Board2ekMatch}, IniFileName, Board, Device, Field, Value, Index, MaxDevices, Chrono2ekStr, LocalPartNumber).

get_device_index_from_cfg_search_for_device_new({ok, DevicesValueStr}, {ok, FieldValueStr}, {ok, match}, _IniFileName, _Board, Device, _Field, Value, Index, _MaxDevices, _Chrono2ekStr, _LocalPartNumber) when
    (DevicesValueStr == Device) and (FieldValueStr == Value) ->
        {ok, Index};
get_device_index_from_cfg_search_for_device_new({_Result1, _DevicesValueStr}, {_Result2, _FieldValueStr}, {_Result3, _Board2ekMatch}, IniFileName, Board, Device, Field, Value, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
     get_device_index_from_cfg_search_for_device_new(IniFileName, Board, Device, Field, Value, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber).


%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec get_device_index_from_cfg(string, string, string, string, string) -> {result, number}.
get_device_index_from_cfg(IniFileName, Board, Device, Field, Value) ->
    {Result, NumDevicesStr} = read_cfg_file(IniFileName, Board, "num_devices"),
    get_device_index_from_cfg({Result, NumDevicesStr}, IniFileName, Board, Device, Field, Value).

get_device_index_from_cfg({ok, NumDevicesStr}, IniFileName, Board, Device, Field, Value) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, 1, NumDevices);
get_device_index_from_cfg({_Result, _NumDevicesStr}, _IniFileName, _Board, _Device, _Field, _Value) ->
    {error, -1}.

get_device_index_from_cfg_search_for_device(_IniFileName, _Board, _Device, _Field, _Value, Index, MaxDevices) when (Index > MaxDevices) ->
    {error, -1};
get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, Index, MaxDevices) ->
    {Result1, DevicesValueStr} = read_cfg_file(IniFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, FieldValueStr} = read_cfg_file(IniFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)])),
    get_device_index_from_cfg_search_for_device({Result1, DevicesValueStr}, {Result2, FieldValueStr}, IniFileName, Board, Device, Field, Value, Index, MaxDevices).

get_device_index_from_cfg_search_for_device({ok, DevicesValueStr}, {ok, FieldValueStr}, _IniFileName, _Board, Device, _Field, Value, Index, _MaxDevices) when
    (DevicesValueStr == Device) and (FieldValueStr == Value) ->
        {ok, Index};
get_device_index_from_cfg_search_for_device({_Result1, _DevicesValueStr}, {_Result2, _FieldValueStr}, IniFileName, Board, Device, Field, Value, Index, MaxDevices) ->
     get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, Index + 1, MaxDevices).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec get_file_image_name(string, string, string, string) -> {result, string}.
get_file_image_name(CfgFileName, Board, Device, InputFile) when (InputFile == "") ->
    {Result1, File} = read_field_from_cfg(CfgFileName, Board, Device, "file"),
    {Result2, Md5} = read_field_from_cfg(CfgFileName, Board, Device, "md5"),
    get_file_image_name({Result1, File}, {Result2, Md5}, CfgFileName, Board, Device, InputFile);
get_file_image_name(_IniFile, _Board, _Device, InputFile) ->
    updater_hw_devices_utils:check_file_exists(InputFile).

get_file_image_name({ok, File}, {ok, Md5}, _IniFile, _Board, _Device, _InputFile) ->
    {Result3, Msg} = updater_hw_devices_utils:md5_check(File, Md5),
    get_file_image_name({Result3, Msg}, File);
get_file_image_name({_Result1, _File}, {_Result2, _Md5}, _IniFile, _Board, _Device, _InputFile) ->
    {error, "can not read ini cfg file"}.

get_file_image_name({ok, _Msg}, File) ->
    {ok, File};
get_file_image_name({_Result3, Msg}, _File) ->
    {error, Msg}.

%-----------------------------------------------------------------------------
%
%
%   io:format("~p ~p ~p ~n", [DevicesStr, EnabledStr, CheckversionStr]),
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec check_for_supported_devices_new(string, string, string, string, string, string) -> {result, string}.
check_for_supported_devices_new(CfgFileName, Board, Device, BaseBoard, Active, LocalPartNumber) ->
    {Result1, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {Result2, Chrono2ekStr} = read_cfg_file(CfgFileName, Board, "chrono_2ek"),
    check_for_supported_devices_new({Result1, NumDevicesStr}, {Result2, Chrono2ekStr}, CfgFileName, Board, Device, BaseBoard, Active, LocalPartNumber).

check_for_supported_devices_new({ok, NumDevicesStr}, {ok, Chrono2ekStr}, CfgFileName, Board, Device, BaseBoard, Active, LocalPartNumber) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    check_for_supported_devices_search_for_device_new(CfgFileName, Board, Device, BaseBoard, Active, 1, NumDevices, Chrono2ekStr, LocalPartNumber);
check_for_supported_devices_new({_Result, _NumDevicesStr}, {_Result2, _Chrono2ekStr}, _IniFile, _Board, _Device, _BaseBoard, _Active, _LocalPartNumber) ->
    error.

check_for_supported_devices_search_for_device_new(_IniFile, _Board, _Device, _BaseBoard, _Active, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    error;
check_for_supported_devices_search_for_device_new(CfgFileName, Board, Device, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    {Result1, DevicesStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, ActivecardStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
    {Result3, VfVrIcs2ekStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    check_for_supported_devices_search_for_device_new({Result1, DevicesStr}, {Result2, ActivecardStr}, {Result3, Board2ekMatch}, CfgFileName, Board, Device, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber).

check_for_supported_devices_search_for_device_new({ok, DevicesStr}, {ok, ActivecardStr}, {ok, match}, _IniFile, Board, Device, BaseBoard, Active, _Index, _MaxDevices, _Chrono2ekStr, _LocalPartNumber) when
    ((BaseBoard == Board) or (ActivecardStr == Active)) and (DevicesStr == Device) ->
        ok;
check_for_supported_devices_search_for_device_new({_Result1, _DevicesStr}, {_Result2, _ActivecardStr}, {_Result3, _Board2ekMatch}, CfgFileName, Board, Device, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    check_for_supported_devices_search_for_device_new(CfgFileName, Board, Device, BaseBoard, Active, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber).

%-----------------------------------------------------------------------------
%
%
%   io:format("~p ~p ~p ~n", [DevicesStr, EnabledStr, CheckversionStr]),
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec check_for_supported_devices(string, string, string, string, string) -> {result, string}.
check_for_supported_devices(CfgFileName, Board, Device, BaseBoard, Active) ->
    {Result, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    check_for_supported_devices({Result, NumDevicesStr}, CfgFileName, Board, Device, BaseBoard, Active).

check_for_supported_devices({ok, NumDevicesStr}, CfgFileName, Board, Device, BaseBoard, Active) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    check_for_supported_devices_search_for_device(CfgFileName, Board, Device, BaseBoard, Active, 1, NumDevices);
check_for_supported_devices({_Result, _NumDevicesStr}, _IniFile, _Board, _Device, _BaseBoard, _Active) ->
    error.

check_for_supported_devices_search_for_device(_IniFile, _Board, _Device, _BaseBoard, _Active, Index, MaxDevices) when (Index > MaxDevices) ->
    error;
check_for_supported_devices_search_for_device(CfgFileName, Board, Device, BaseBoard, Active, Index, MaxDevices) ->
    {Result1, DevicesStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, ActivecardStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
    check_for_supported_devices_search_for_device({Result1, DevicesStr}, {Result2, ActivecardStr}, CfgFileName, Board, Device, BaseBoard, Active, Index, MaxDevices).

check_for_supported_devices_search_for_device({ok, DevicesStr}, {ok, ActivecardStr}, _IniFile, Board, Device, BaseBoard, Active, _Index, _MaxDevices) when
    ((BaseBoard == Board) or (ActivecardStr == Active)) and (DevicesStr == Device) ->
        ok;
check_for_supported_devices_search_for_device({_Result1, _DevicesStr}, {_Result2, _ActivecardStr}, CfgFileName, Board, Device, BaseBoard, Active, Index, MaxDevices) ->
    check_for_supported_devices_search_for_device(CfgFileName, Board, Device, BaseBoard, Active, Index + 1, MaxDevices).


%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec enable_disable_device_new(string, string, string, string, string, string) -> ok | error.
enable_disable_device_new(CfgFileName, BoardDeviceAlias, NewState, BaseBoard, Active, LocalPartNumber) ->
    Board = updater_hw_devices_utils:extract_board(BoardDeviceAlias),
    Device = updater_hw_devices_utils:extract_device (BoardDeviceAlias),
    SupportedDevice = check_for_supported_devices(CfgFileName, Board, Device, BaseBoard, Active),
    enable_disable_device_new(SupportedDevice, CfgFileName, Board, Device, BoardDeviceAlias, NewState, LocalPartNumber).

enable_disable_device_new(ok, CfgFileName, Board, Device, BoardDeviceAlias, NewState, LocalPartNumber) ->
    {Result1, Num_devicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {Result2, Dependencies1} = read_field_from_cfg_anyway(CfgFileName, Board, Device, "dependencies"),
    {Result3, Chrono2ekStr} = read_cfg_file(CfgFileName, Board, "chrono_2ek"),
    enable_disable_device_new({Result1, Num_devicesStr}, {Result2, Dependencies1}, {Result3, Chrono2ekStr}, CfgFileName, Board, Device, BoardDeviceAlias, NewState, LocalPartNumber);
enable_disable_device_new(_SupportedDevice, _IniFile, _Board, _Device, _BoardDeviceAlias, _NewState, _LocalPartNumber) ->
    {error, ""}.

enable_disable_device_new({ok, Num_devicesStr}, {ok, Dependencies1}, {ok, Chrono2ekStr}, CfgFileName, Board, Device, BoardDeviceAlias, NewState, LocalPartNumber) ->
    Device_dependent = updater_hw_devices_utils:extract_device(Dependencies1),
    Alias = updater_hw_devices_utils:extract_alias (BoardDeviceAlias),
    {Num_devices, _} = string:to_integer(Num_devicesStr),
    disable_device_new(CfgFileName, Board, Device, Device_dependent, 1, Num_devices, Chrono2ekStr, LocalPartNumber),
    enable_device_new(CfgFileName, Board, Device, Device_dependent, Alias, NewState, 1, Num_devices, Chrono2ekStr, LocalPartNumber);
enable_disable_device_new({_Result1, _Num_devicesStr}, {_Result2, _Dependencies1}, {_Result3, _Chrono2ekStr}, _IniFile, _Board, _Device, _BoardDeviceAlias, _NewState, _LocalPartNumber) ->
    {error, ""}.

disable_device_new(_IniFile, _Board, _Device, _Device_dependent, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    {ok, ""};
disable_device_new(CfgFileName, Board, Device, Device_dependent, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    {Result1, Device1} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, VfVrIcs2ekStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    disable_device_new({Result1, Device1}, {Result2, Board2ekMatch}, CfgFileName, Board, Device, Device_dependent, Index, MaxDevices,  Chrono2ekStr, LocalPartNumber).

disable_device_new({ok, Device1}, {ok, match}, CfgFileName, Board, Device, Device_dependent, Index, MaxDevices,  Chrono2ekStr, LocalPartNumber) when
    ((Device1 == Device) or (Device1 == Device_dependent)) ->
        read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), "0", wr),
        disable_device_new(CfgFileName, Board, Device, Device_dependent, Index + 1, MaxDevices,  Chrono2ekStr, LocalPartNumber);
disable_device_new({_Result, _Device1}, {_Result2, _Board2ekMatch}, CfgFileName, Board, Device, Device_dependent, Index, MaxDevices,  Chrono2ekStr, LocalPartNumber) ->
    disable_device_new(CfgFileName, Board, Device, Device_dependent, Index + 1, MaxDevices,  Chrono2ekStr, LocalPartNumber).

enable_device_new(_IniFile, _Board, _Device, _Device_dependent, _Alias, _NewState, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    {ok, ""};
enable_device_new(CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    {Result1, Device1} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Alias1} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
    {Result3, VfVrIcs2ekStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    enable_device_new({Result1, Device1}, {Result2, Alias1}, {Result3, Board2ekMatch}, CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices, Chrono2ekStr, LocalPartNumber).

enable_device_new({ok, Device1}, {ok, Alias1}, {ok, match}, CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) when
    ((Device1 == Device) or (Device1 == Device_dependent)) and (Alias == Alias1) ->
        read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), NewState, wr),
        enable_device_new(CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber);
enable_device_new({_Result1, _Device1}, {_Result2, _Alias1}, {_Result3, _Board2ekMatch}, CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    enable_device_new(CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber).


%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec enable_disable_device(string, string, string, string, string) -> ok | error.
enable_disable_device(CfgFileName, BoardDeviceAlias, NewState, BaseBoard, Active) ->
    Board = updater_hw_devices_utils:extract_board(BoardDeviceAlias),
    Device = updater_hw_devices_utils:extract_device (BoardDeviceAlias),
    SupportedDevice = check_for_supported_devices(CfgFileName, Board, Device, BaseBoard, Active),
    enable_disable_device(SupportedDevice, CfgFileName, Board, Device, BoardDeviceAlias, NewState).

enable_disable_device(ok, CfgFileName, Board, Device, BoardDeviceAlias, NewState) ->
    {Result1, Num_devicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {Result2, Dependencies1} = read_field_from_cfg_anyway(CfgFileName, Board, Device, "dependencies"),
    enable_disable_device({Result1, Num_devicesStr}, {Result2, Dependencies1}, CfgFileName, Board, Device, BoardDeviceAlias, NewState);
enable_disable_device(_SupportedDevice, _IniFile, _Board, _Device, _BoardDeviceAlias, _NewState) ->
    {error, ""}.

enable_disable_device({ok, Num_devicesStr}, {ok, Dependencies1}, CfgFileName, Board, Device, BoardDeviceAlias, NewState) ->
    Device_dependent = updater_hw_devices_utils:extract_device(Dependencies1),
    Alias = updater_hw_devices_utils:extract_alias (BoardDeviceAlias),
    {Num_devices, _} = string:to_integer(Num_devicesStr),
    disable_device(CfgFileName, Board, Device, Device_dependent, 1, Num_devices),
    enable_device(CfgFileName, Board, Device, Device_dependent, Alias, NewState, 1, Num_devices);
enable_disable_device({_Result1, _Num_devicesStr}, {_Result2, _Dependencies1}, _IniFile, _Board, _Device, _BoardDeviceAlias, _NewState) ->
    {error, ""}.

disable_device(_IniFile, _Board, _Device, _Device_dependent, Index, MaxDevices) when (Index > MaxDevices) ->
    {ok, ""};
disable_device(CfgFileName, Board, Device, Device_dependent, Index, MaxDevices) ->
    {Result, Device1} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    disable_device({Result, Device1}, CfgFileName, Board, Device, Device_dependent, Index, MaxDevices).

disable_device({ok, Device1}, CfgFileName, Board, Device, Device_dependent, Index, MaxDevices) when
    ((Device1 == Device) or (Device1 == Device_dependent)) ->
        read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), "0", wr),
        disable_device(CfgFileName, Board, Device, Device_dependent, Index + 1, MaxDevices);
disable_device({_Result, _Device1}, CfgFileName, Board, Device, Device_dependent, Index, MaxDevices) ->
    disable_device(CfgFileName, Board, Device, Device_dependent, Index + 1, MaxDevices).

enable_device(_IniFile, _Board, _Device, _Device_dependent, _Alias, _NewState, Index, MaxDevices) when (Index > MaxDevices) ->
    {ok, ""};
enable_device(CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) ->
    {Result1, Device1} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Alias1} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
    enable_device({Result1, Device1}, {Result2, Alias1},CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices).

enable_device({ok, Device1}, {ok, Alias1},CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) when
    ((Device1 == Device) or (Device1 == Device_dependent)) and (Alias == Alias1) ->
        read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), NewState, wr),
        enable_device(CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices);
enable_device({_Result1, _Device1}, {_Result2, _Alias1},CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) ->
    enable_device(CfgFileName, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices).

%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec show_devices_new(string, string, string, string) -> {result, string}.
show_devices_new(CfgFileName, BaseBoard, Active, _LocalPartNumber) when (CfgFileName == ""); (BaseBoard == ""); (Active == "") ->
    io:format("Warning: No board was identified!~n");
show_devices_new(CfgFileName, BaseBoard, Active, LocalPartNumber) ->
    {Result, NumBoardsStr} = read_cfg_file(CfgFileName, "boards", "num_boards"),
    show_devices_new({Result, NumBoardsStr}, CfgFileName, BaseBoard, Active, LocalPartNumber).
show_devices_new({ok, NumBoardsStr}, CfgFileName, BaseBoard, Active, LocalPartNumber) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    show_devices_next_board_new(CfgFileName, BaseBoard, Active, 1, NumBoards, LocalPartNumber);
show_devices_new({_Result, _NumBoardsStr}, _IniFile, _BaseBoard, _Active,_LocalPartNumber) ->
    error.

show_devices_next_board_new(_IniFile, _BaseBoard, _Active, Index, MaxBoards, _LocalPartNumber) when (Index > MaxBoards) ->
    ok;
show_devices_next_board_new(CfgFileName, BaseBoard, Active, Index, MaxBoards, LocalPartNumber) ->
    {Result1, Board} = read_cfg_file(CfgFileName, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    {Result2, Chrono2ekStr} = read_cfg_file(CfgFileName, Board, "chrono_2ek"),
    show_devices_next_board_new({Result1, Board},  {Result2, Chrono2ekStr}, CfgFileName, BaseBoard, Active, Index, MaxBoards, LocalPartNumber).
show_devices_next_board_new({ok, Board}, {ok, Chrono2ekStr}, CfgFileName, BaseBoard, Active, Index, MaxBoards, LocalPartNumber) ->
    {_, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    show_devices_next_device_new(CfgFileName, Board, BaseBoard, Active, 1, NumDevices, Chrono2ekStr, LocalPartNumber),
    show_devices_next_board_new(CfgFileName, BaseBoard, Active, Index + 1, MaxBoards, LocalPartNumber);
show_devices_next_board_new({_Result1, _Board}, {_Result2, _Chrono2ekStr}, _IniFile, _BaseBoard, _Active, _Index, _MaxBoards, _LocalPartNumber) ->
    error.

show_devices_next_device_new(_IniFile, _Board, _BaseBoard, _Active, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    ok;
show_devices_next_device_new(CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    {Result1, Device} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Activecard} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
    {Result3, Enabled} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    {Result4, VfVrIcs2ekStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    show_devices_next_device_new({Result1, Device}, {Result2, Activecard}, {Result3, Enabled}, {Result4, Board2ekMatch}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber).
show_devices_next_device_new({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, match}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") ->
    io:format("                ~s_~s~n", [Board, Device]),
    show_devices_next_device_new(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber);
show_devices_next_device_new({_Result1, _Device}, {_Result2, _Activecard}, {_Result3, _Enabled}, {_Result4, _Board2ekMatch}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    show_devices_next_device_new(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber).


%-----------------------------------------------------------------------------
%
% ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec show_devices(string, string, string) -> {result, string}.
show_devices(CfgFileName, BaseBoard, Active) when (CfgFileName == ""); (BaseBoard == ""); (Active == "") ->
    io:format("Warning: No board was identified!~n");
show_devices(CfgFileName, BaseBoard, Active) ->
    {Result, NumBoardsStr} = read_cfg_file(CfgFileName, "boards", "num_boards"),
    show_devices({Result, NumBoardsStr}, CfgFileName, BaseBoard, Active).
show_devices({ok, NumBoardsStr}, CfgFileName, BaseBoard, Active) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    show_devices_next_board(CfgFileName, BaseBoard, Active, 1, NumBoards);
show_devices({_Result, _NumBoardsStr}, _IniFile, _BaseBoard, _Active) ->
    error.

show_devices_next_board(_IniFile, _BaseBoard, _Active, Index, MaxBoards) when (Index > MaxBoards) ->
    ok;
show_devices_next_board(CfgFileName, BaseBoard, Active, Index, MaxBoards) ->
    {Result, Board} = read_cfg_file(CfgFileName, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    show_devices_next_board({Result, Board}, CfgFileName, BaseBoard, Active, Index, MaxBoards).
show_devices_next_board({ok, Board}, CfgFileName, BaseBoard, Active, Index, MaxBoards) ->
    {_, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    show_devices_next_Device(CfgFileName, Board, BaseBoard, Active, 1, NumDevices),
    show_devices_next_board(CfgFileName, BaseBoard, Active, Index + 1, MaxBoards);
show_devices_next_board({_Result, _Board}, _IniFile, _BaseBoard, _Active, _Index, _MaxBoards) ->
    error.

show_devices_next_Device(_IniFile, _Board, _BaseBoard, _Active, Index, MaxDevices) when (Index > MaxDevices) ->
    ok;
show_devices_next_Device(CfgFileName, Board, BaseBoard, Active, Index, MaxDevices) ->
    {Result1, Device} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Activecard} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
    {Result3, Enabled} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    show_devices_next_Device({Result1, Device}, {Result2, Activecard}, {Result3, Enabled}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices).
show_devices_next_Device({ok, Device}, {ok, Activecard}, {ok, Enabled}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") ->
    io:format("                ~s_~s~n", [Board, Device]),
    show_devices_next_Device(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices);
show_devices_next_Device({_Result1, _Device}, {_Result2, _Activecard}, {_Result3, _Enabled}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices) ->
    show_devices_next_Device(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices).


%-----------------------------------------------------------------------------
%
%  ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec show_boards_tree_new(string, string, string, string) -> ok | error.
show_boards_tree_new(CfgFileName, BaseBoard, Active, LocalPartNumber) ->
    {Result, NumBoardsStr} = read_cfg_file(CfgFileName, "boards", "num_boards"),
    show_boards_tree_new({Result, NumBoardsStr}, CfgFileName, BaseBoard, Active, LocalPartNumber).
show_boards_tree_new({ok, NumBoardsStr}, CfgFileName, BaseBoard, Active, LocalPartNumber) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    show_boards_tree_next_board_new(CfgFileName, BaseBoard, Active, 1, NumBoards, LocalPartNumber);
show_boards_tree_new({_Result, _NumBoardsStr}, _IniFile, _BaseBoard, _Active, _LocalPartNumber) ->
    error.

show_boards_tree_next_board_new(_IniFile, _BaseBoard, _Active, Index, MaxBoards, _LocalPartNumber) when (Index >MaxBoards) ->
    ok;
show_boards_tree_next_board_new(CfgFileName, BaseBoard, Active, Index, MaxBoards, LocalPartNumber) ->
    {Result1, Board} = read_cfg_file(CfgFileName, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    {Result2, Chrono2ekStr} = read_cfg_file(CfgFileName, Board, "chrono_2ek"),
    show_boards_tree_next_board_new({Result1, Board}, {Result2, Chrono2ekStr}, CfgFileName, BaseBoard, Active, Index, MaxBoards, LocalPartNumber).
show_boards_tree_next_board_new({ok, Board}, {ok, Chrono2ekStr}, CfgFileName, BaseBoard, Active, Index, MaxBoards, LocalPartNumber) ->
    {_, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    show_boards_tree_next_device_new(CfgFileName, Board, BaseBoard, Active, 1, NumDevices, 0, Chrono2ekStr, LocalPartNumber),
    show_boards_tree_next_board_new(CfgFileName, BaseBoard, Active, Index + 1, MaxBoards, LocalPartNumber);
show_boards_tree_next_board_new({_Result1, _Board}, {_Result2, _Chrono2ekStr}, _IniFile, _BaseBoard, _Active, _Index, _MaxBoards, _LocalPartNumber) ->
    error.


show_boards_tree_next_device_new(_IniFile, _Board, _BaseBoard, _Active, Index, MaxDevices, _Flag, _Chrono2ekStr, _LocalPartNumber) when (Index > MaxDevices) ->
    ok;
show_boards_tree_next_device_new(CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber) ->
     {Result1, Device} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
     {Result2, Activecard} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
     {Result3, Enabled} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
     {Result4, Alias} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
     {Result5, VfVrIcs2ekStr} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
    Board2ekMatch = board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
    show_boards_tree_next_device_new({Result1, Device}, {Result2, Activecard}, {Result3, Enabled}, {Result4, Alias}, {Result5, Board2ekMatch}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber).
show_boards_tree_next_device_new({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, {ok, match}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias == "") and (Flag == 0) ->
    io:format("~n"),
    io:format("             ~s~n", [Board]),
    io:format("              +-- ~s~n", [Device]),
    show_boards_tree_next_device_new(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1, Chrono2ekStr, LocalPartNumber);
show_boards_tree_next_device_new({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, {ok, match}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias == "") and (Flag /= 0) ->
    io:format("              +-- ~s~n", [Device]),
    show_boards_tree_next_device_new(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1, Chrono2ekStr, LocalPartNumber);
show_boards_tree_next_device_new({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, {ok, match}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias /= "") and (Flag == 0) ->
    io:format("~n"),
    io:format("             ~s~n", [Board]),
    io:format("              +-- ~s (~s)~n", [Device, Alias]),
    show_boards_tree_next_device_new(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1, Chrono2ekStr, LocalPartNumber);
show_boards_tree_next_device_new({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, {ok, match}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias /= "") and (Flag /= 0) ->
    io:format("              +-- ~s (~s)~n", [Device, Alias]),
    show_boards_tree_next_device_new(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1,Chrono2ekStr, LocalPartNumber);
show_boards_tree_next_device_new({_Result1, _Device}, {_Result2, _Activecard}, {_Result3, _Enabled}, {_Result4, _Alias}, {_Result5, _Board2ekMatch}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber) ->
    show_boards_tree_next_device_new(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag, Chrono2ekStr, LocalPartNumber).


%-----------------------------------------------------------------------------
%
%  ToDo: to include 2ek
%-----------------------------------------------------------------------------
-spec show_boards_tree(string, string, string) -> ok | error.
show_boards_tree(CfgFileName, BaseBoard, Active) ->
    {Result, NumBoardsStr} = read_cfg_file(CfgFileName, "boards", "num_boards"),
    show_boards_tree({Result, NumBoardsStr}, CfgFileName, BaseBoard, Active).
show_boards_tree({ok, NumBoardsStr}, CfgFileName, BaseBoard, Active) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    show_boards_tree_next_board(CfgFileName, BaseBoard, Active, 1, NumBoards);
show_boards_tree({_Result, _NumBoardsStr}, _IniFile, _BaseBoard, _Active) ->
    error.

show_boards_tree_next_board(_IniFile, _BaseBoard, _Active, Index, MaxBoards) when (Index >MaxBoards) ->
    ok;
show_boards_tree_next_board(CfgFileName, BaseBoard, Active, Index, MaxBoards) ->
    {Result, Board} = read_cfg_file(CfgFileName, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    show_boards_tree_next_board({Result, Board}, CfgFileName, BaseBoard, Active, Index, MaxBoards).
show_boards_tree_next_board({ok, Board}, CfgFileName, BaseBoard, Active, Index, MaxBoards) ->
    {_, NumDevicesStr} = read_cfg_file(CfgFileName, Board, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    show_boards_tree_next_device(CfgFileName, Board, BaseBoard, Active, 1, NumDevices, 0),
    show_boards_tree_next_board(CfgFileName, BaseBoard, Active, Index + 1, MaxBoards);
show_boards_tree_next_board({_Result, _Board}, _IniFile, _BaseBoard, _Active, _Index, _MaxBoards) ->
    error.


show_boards_tree_next_device(_IniFile, _Board, _BaseBoard, _Active, Index, MaxDevices, _Flag) when (Index > MaxDevices) ->
    ok;
show_boards_tree_next_device(CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag) ->
     {Result1, Device} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
     {Result2, Activecard} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
     {Result3, Enabled} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
     {Result4, Alias} = read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
     show_boards_tree_next_device({Result1, Device}, {Result2, Activecard}, {Result3, Enabled}, {Result4, Alias}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag).
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias == "") and (Flag == 0) ->
    io:format("~n"),
    io:format("             ~s~n", [Board]),
    io:format("              +-- ~s~n", [Device]),
    show_boards_tree_next_device(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias == "") and (Flag /= 0) ->
    io:format("              +-- ~s~n", [Device]),
    show_boards_tree_next_device(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias /= "") and (Flag == 0) ->
    io:format("~n"),
    io:format("             ~s~n", [Board]),
    io:format("              +-- ~s (~s)~n", [Device, Alias]),
    show_boards_tree_next_device(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias /= "") and (Flag /= 0) ->
    io:format("              +-- ~s (~s)~n", [Device, Alias]),
    show_boards_tree_next_device(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({_Result1, _Device}, {_Result2, _Activecard}, {_Result3, _Enabled}, {_Result4, _Alias}, CfgFileName, Board, BaseBoard, Active, Index, MaxDevices, Flag) ->
    show_boards_tree_next_device(CfgFileName, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec check_for_supported_board(string, string) -> ok | error.
check_for_supported_board(CfgFileName, Board) ->
    check_for_supported_board(CfgFileName, read_cfg_file(CfgFileName, "boards", "num_boards"), Board).

check_for_supported_board(CfgFileName, {ok, NumBoardsStr}, Board) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    check_for_supported_board(CfgFileName, Board, 1, NumBoards);
check_for_supported_board(_IniFile, {error,_}, _Board) ->
    error.

check_for_supported_board(_IniFile, _Board, Index, MaxBoards) when Index > MaxBoards ->
    error;
check_for_supported_board(CfgFileName, Board, Index, MaxBoards) ->
    {Result, BoardRead} = read_cfg_file(CfgFileName, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    check_for_supported_board_next(CfgFileName, {Result, BoardRead}, Board, Index, MaxBoards).

check_for_supported_board_next(_IniFile, {ok, Board}, Board, _Index, _MaxBoards) ->
    ok;
check_for_supported_board_next(_IniFile, {error, _BoardRead}, _Board, _Index, _MaxBoards) ->
    error;
check_for_supported_board_next(CfgFileName, _Result, Board, Index, MaxBoards) ->
    check_for_supported_board(CfgFileName, Board, Index + 1, MaxBoards).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec update_cfg_file(string, string) -> {result, string}.
update_cfg_file(Source, Target) ->
    {Result, NumBoardsStr} = read_cfg_file(Source, "boards", "num_boards"),
    update_cfg_file({Result, NumBoardsStr}, Source, Target).
update_cfg_file({ok, NumBoardsStr}, Source, Target) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    update_cfg_file_next_board(Source, Target, 1, NumBoards);
update_cfg_file({_, _NumBoardsStr}, _Source, _Target) ->
    error.

update_cfg_file_next_board(_Source, _Target, Index, MaxBoards) when (Index > MaxBoards) ->
    ok;
update_cfg_file_next_board(Source, Target, Index, MaxBoards) ->
    {Result, BoardRead} = read_cfg_file(Source, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    update_cfg_file_next_board({Result, BoardRead}, Source, Target, Index, MaxBoards).
update_cfg_file_next_board({ok, BoardRead}, Source, Target, Index, MaxBoards) ->
    {_, NumDevicesStr} = read_cfg_file(Source, BoardRead, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    update_cfg_file_next_device(Source, Target, BoardRead, 1, NumDevices),
    update_cfg_file_next_board(Source, Target, Index + 1, MaxBoards);
update_cfg_file_next_board({_, _BoardRead}, _Source, _Target, _Index, _MaxBoards) ->
    error.

update_cfg_file_next_device(_Source, _Target, _BoardRead, Index, MaxDevices) when (Index > MaxDevices) ->
    ok;
update_cfg_file_next_device(Source, Target, BoardRead, Index, MaxDevices) ->
    {Result1, Device} = read_cfg_file(Source, BoardRead, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Enabled} = read_cfg_file(Source, BoardRead, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    {Result3, Alias} = read_cfg_file(Source, BoardRead, unicode:characters_to_list(["alias", integer_to_list(Index)])),
    update_cfg_file_next_device({Result1, Device}, {Result2, Enabled}, {Result3, Alias}, Source, Target, BoardRead, Index, MaxDevices).
update_cfg_file_next_device({ok, Device}, {ok, Enabled}, {ok, Alias}, Source, Target, BoardRead, Index, MaxDevices) ->
    {_, IndexTarget} = get_device_index_from_cfg(Target, BoardRead, Device, "alias", Alias),
    read_cfg_file(Target, BoardRead, unicode:characters_to_list(["enabled", integer_to_list(IndexTarget)]), Enabled, wr),
    update_cfg_file_next_device(Source, Target, BoardRead, Index + 1, MaxDevices);
update_cfg_file_next_device({_Result1, _Device}, {_Result2, _Enabled}, {_Result3, _Alias}, _Source, _Target, _BoardRead, _Index, _MaxDevices) ->
    error.
