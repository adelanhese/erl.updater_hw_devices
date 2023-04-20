-module(updater_hw_devices_cfgfileparse).
-include("updater_hw_devices_defines.hrl").

% Already tested and working
-export([ini_file/3,
         ini_file/5,
         read_field_from_cfg/4,
         read_field_from_cfg_anyway/4,
         get_device_index_from_cfg/5,
         get_file_image_name/4,
         check_for_supported_devices/5,
         enable_disable_device/5,
         show_devices/3,
         show_boards_tree/3,
         check_for_supported_board/2,
         update_cfg_file/2]).

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec ini_file(string, string, string) -> {status, string}.
ini_file(IniFile, Sector, Field) ->
    ini_file(IniFile, Sector, Field, "", rd).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec ini_file(string, string, string, string, [rd|wr]) -> {status, string}.
ini_file(IniFile, Sector, Field, NewFieldValue, Oper) ->
    {Status, FileData} = file:read_file(IniFile),
    ini_file_to_list({Status, FileData}, IniFile, Sector, Field, NewFieldValue, Oper).

ini_file_to_list({ok, FileData}, IniFile, Sector, Field, NewFieldValue, Oper) ->
    List = string:tokens(binary_to_list(FileData), "\n"),
    {Result, NextIndex} = ini_file_search_for_sector(List, 1, Sector),
    ini_file_search_for_first_field(IniFile, List, {Result, NextIndex}, Sector, Field, NewFieldValue, Oper);
ini_file_to_list({_Status, _FileData}, _IniFile, _Sector, _Field, _NewFieldValue, _Oper) ->
    {error, "Fail to access the configuration file"}.
    
ini_file_search_for_first_field(IniFile, List, {true, NextIndex}, _Sector, Field, NewFieldValue, Oper) ->
    ini_file_search_for_field(IniFile, List, NextIndex + 1, Field, NewFieldValue, Oper);
ini_file_search_for_first_field(IniFile, List, {_Result, _NextIndex}, Sector, Field, NewFieldValue, Oper) ->
    ini_file_add_new_sector_and_field(IniFile, List, Sector, Field, NewFieldValue, Oper).

ini_file_add_new_sector_and_field(IniFile, List, Sector, Field, NewFieldValue, wr) ->
     NewSector = unicode:characters_to_list(["[", Sector, "]"]),
     NewField = unicode:characters_to_list([Field, " = ", NewFieldValue]),
     List1 = lists:append(List, [NewSector]),
     List2 = lists:append(List1, [NewField]),
     Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List2]),
     file:write_file(IniFile, Text),
     {ok, "New Sector and field added at end of file"};
ini_file_add_new_sector_and_field(_IniFile, _List, _Sector, _Field, _NewFieldValue, _Oper) ->
     {error, "Sector not found"}.

%------------------------------------------
-spec ini_file_search_for_sector([list], number, string) -> {status, number}.
ini_file_search_for_sector(List, Index, Sector) when (Index < length(List)), (Index > 0) ->
    CurrentSectorPosition = string:str(lists:nth(Index, List), unicode:characters_to_list(["[", Sector, "]"])),
    ini_file_search_for_sector_next(List, Index, Sector, CurrentSectorPosition);
ini_file_search_for_sector(_List, _Index, _Sector) ->
    {false, -1}.

ini_file_search_for_sector_next(List, Index, Sector, 0) ->
    ini_file_search_for_sector(List, Index + 1, Sector);
ini_file_search_for_sector_next(_List, Index, _Sector, _CurrentSectorPosition) ->
    {true, Index}.

%---------------------------------------------
ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper) when (Index > length(List)); (Index < 0)->
    ini_file_add_new_field(IniFile, List, Index, Field, NewFieldValue, Oper);
ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper) ->
    CurrentField = lists:nth(Index, List),
    EndOfSector = string:str(CurrentField, "["),
    ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper, CurrentField, EndOfSector).

ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper, _CurrentField, EndOfSector) when (EndOfSector > 0) ->
    ini_file_add_new_field(IniFile, List, Index, Field, NewFieldValue, Oper);
ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper, CurrentField, _EndOfSector) ->
    FieldSeparator = string:str(CurrentField, unicode:characters_to_list([Field, " = "])),
    ini_file_search_for_field_next(IniFile, List, Index, Field, NewFieldValue, Oper, CurrentField, FieldSeparator).

ini_file_search_for_field_next(IniFile, List, Index, Field, NewFieldValue, Oper, _CurrentField, 0) ->
    ini_file_search_for_field(IniFile, List, Index + 1, Field, NewFieldValue, Oper);
ini_file_search_for_field_next(IniFile, List, Index, Field, NewFieldValue, Oper, CurrentField, _FieldSeparator) ->
    CurrentFieldValue = updater_hw_devices_utils:extract_field(CurrentField),
    ini_file_replace_field(IniFile, List, Index, Field, CurrentFieldValue, NewFieldValue, Oper).

%--------------------------------------
ini_file_add_new_field(IniFile, List, Index, Field, FieldValue, Oper) when (Oper == wr) ->
    ini_file_add_new_field(IniFile, List, Index, Field, FieldValue);
ini_file_add_new_field(_IniFile, _List, _Index, _Field, _FieldValue, _Oper) ->
    {error, "Field not found"}.

ini_file_add_new_field(IniFile, List, Index, Field, FieldValue) when (Index > length(List)) ->
    NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
    List1 = lists:append(List, [NewField]),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(IniFile, Text),
    {Result, "Creating new field at end of file"};
ini_file_add_new_field(IniFile, List, Index, Field, FieldValue) ->
    NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
    List1 = updater_hw_devices_utils:list_insert(List, Index, NewField),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(IniFile, Text),
    {Result, "Creating new field at end of sector"}.

%--------------------------------------
ini_file_replace_field(IniFile, List, Index, Field, _CurrentFieldValue, NewFieldValue, Oper) when (Oper == wr) ->
    NewField = unicode:characters_to_list([Field, " = ", NewFieldValue]),
    List1 = updater_hw_devices_utils:list_replace(List, Index, NewField),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(IniFile, Text),
    {Result, "Updating the field"};
ini_file_replace_field(_IniFile, _List, _Index, _Field, CurrentFieldValue, _NewFieldValue, _Oper) ->
    {ok, CurrentFieldValue}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_field_from_cfg(string, string, string, string) -> {result, string}.
read_field_from_cfg(IniFile, Board, Device, Field) ->
    {Result, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),
    read_field_from_cfg({Result, NumDevicesStr}, IniFile, Board, Device, Field).

read_field_from_cfg({ok, NumDevicesStr}, IniFile, Board, Device, Field) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    read_field_from_cfg_search_for_device(IniFile, Board, Device, Field, 1, NumDevices);
read_field_from_cfg({_Result, NumDevicesStr}, _IniFile, _Board, _Device, _Field) ->
    {error, NumDevicesStr}.

read_field_from_cfg_search_for_device(_IniFile, _Board, _Device, _Field, Index, MaxDevices) when (Index > MaxDevices) ->
    {error, "field not found"};
read_field_from_cfg_search_for_device(IniFile, Board, Device, Field, Index, MaxDevices) ->
    {Result1, DevicesStr} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, EnabledStr} = ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    read_field_from_cfg_search_for_device({Result1, DevicesStr}, {Result2, EnabledStr}, IniFile, Board, Device, Field, Index, MaxDevices).

read_field_from_cfg_search_for_device({ok, DevicesStr}, {ok, "1"}, IniFile, Board, Device, Field, Index, _MaxDevices) when
    (DevicesStr == Device) ->
        ini_file(IniFile, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));
read_field_from_cfg_search_for_device({_Result1, _DevicesStr}, {_Result2, _EnabledStr}, IniFile, Board, Device, Field, Index, MaxDevices) ->
    read_field_from_cfg_search_for_device(IniFile, Board, Device, Field, Index + 1, MaxDevices).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_field_from_cfg_anyway(string, string, string, string) -> {result, string}.
read_field_from_cfg_anyway(IniFile, Board, Device, Field) ->
    {Result, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),
    read_field_from_cfg_anyway({Result, NumDevicesStr}, IniFile, Board, Device, Field).

read_field_from_cfg_anyway({ok, NumDevicesStr}, IniFile, Board, Device, Field) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    read_field_from_cfg_anyway_search_for_device(IniFile, Board, Device, Field, 1, NumDevices);
read_field_from_cfg_anyway({_Result, NumDevicesStr}, _IniFile, _Board, _Device, _Field) ->
    {error, NumDevicesStr}.

read_field_from_cfg_anyway_search_for_device(_IniFile, _Board, _Device, _Field, Index, MaxDevices) when (Index > MaxDevices) ->
    {error, "field not found"};
read_field_from_cfg_anyway_search_for_device(IniFile, Board, Device, Field, Index, MaxDevices) ->
    {Result1, DevicesStr} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    read_field_from_cfg_anyway_search_for_device({Result1, DevicesStr}, IniFile, Board, Device, Field, Index, MaxDevices).

read_field_from_cfg_anyway_search_for_device({ok, DevicesStr}, IniFile, Board, Device, Field, Index, _MaxDevices) when
    (DevicesStr == Device) ->
           ini_file(IniFile, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));
read_field_from_cfg_anyway_search_for_device({_Result1, _DevicesStr}, IniFile, Board, Device, Field, Index, MaxDevices) ->
    read_field_from_cfg_anyway_search_for_device(IniFile, Board, Device, Field, Index + 1, MaxDevices).



%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec get_device_index_from_cfg(string, string, string, string, string) -> {result, number}.
get_device_index_from_cfg(IniFileName, Board, Device, Field, Value) ->
    {Result, NumDevicesStr} = ini_file(IniFileName, Board, "num_devices"),
    get_device_index_from_cfg({Result, NumDevicesStr}, IniFileName, Board, Device, Field, Value).

get_device_index_from_cfg({ok, NumDevicesStr}, IniFileName, Board, Device, Field, Value) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, 1, NumDevices);
get_device_index_from_cfg({_Result, _NumDevicesStr}, _IniFileName, _Board, _Device, _Field, _Value) ->
    {error, -1}.

get_device_index_from_cfg_search_for_device(_IniFileName, _Board, _Device, _Field, _Value, Index, MaxDevices) when (Index > MaxDevices) ->
    {error, -1};
get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, Index, MaxDevices) ->
    {Result1, DevicesValueStr} = ini_file(IniFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, FieldValueStr} = ini_file(IniFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)])),
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
get_file_image_name(IniFile, Board, Device, InputFile) when (InputFile == "") ->
    {Result1, File} = read_field_from_cfg(IniFile, Board, Device, "file"),
    {Result2, Md5} = read_field_from_cfg(IniFile, Board, Device, "md5"),
    get_file_image_name({Result1, File}, {Result2, Md5}, IniFile, Board, Device, InputFile);
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
%
%-----------------------------------------------------------------------------
-spec check_for_supported_devices(string, string, string, string, string) -> {result, string}.
check_for_supported_devices(IniFile, Board, Device, BaseBoard, Active) ->
    {Result, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),
    check_for_supported_devices({Result, NumDevicesStr}, IniFile, Board, Device, BaseBoard, Active).

check_for_supported_devices({ok, NumDevicesStr}, IniFile, Board, Device, BaseBoard, Active) ->
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    check_for_supported_devices_search_for_device(IniFile, Board, Device, BaseBoard, Active, 1, NumDevices);
check_for_supported_devices({_Result, _NumDevicesStr}, _IniFile, _Board, _Device, _BaseBoard, _Active) ->
    error.

check_for_supported_devices_search_for_device(_IniFile, _Board, _Device, _BaseBoard, _Active, Index, MaxDevices) when (Index > MaxDevices) ->
    error;
check_for_supported_devices_search_for_device(IniFile, Board, Device, BaseBoard, Active, Index, MaxDevices) ->
    {Result1, DevicesStr} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, ActivecardStr} = ini_file(IniFile, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
    check_for_supported_devices_search_for_device({Result1, DevicesStr}, {Result2, ActivecardStr}, IniFile, Board, Device, BaseBoard, Active, Index, MaxDevices).

check_for_supported_devices_search_for_device({ok, DevicesStr}, {ok, ActivecardStr}, _IniFile, Board, Device, BaseBoard, Active, _Index, _MaxDevices) when
    ((BaseBoard == Board) or (ActivecardStr == Active)) and (DevicesStr == Device) ->
        ok;
check_for_supported_devices_search_for_device({_Result1, _DevicesStr}, {_Result2, _ActivecardStr}, IniFile, Board, Device, BaseBoard, Active, Index, MaxDevices) ->
    check_for_supported_devices_search_for_device(IniFile, Board, Device, BaseBoard, Active, Index + 1, MaxDevices).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec enable_disable_device(string, string, string, string, string) -> ok | error.
enable_disable_device(IniFile, BoardDeviceAlias, NewState, BaseBoard, Active) ->
    Board = updater_hw_devices_utils:extract_board(BoardDeviceAlias),
    Device = updater_hw_devices_utils:extract_device (BoardDeviceAlias),
    SupportedDevice = check_for_supported_devices(IniFile, Board, Device, BaseBoard, Active),
    enable_disable_device(SupportedDevice, IniFile, Board, Device, BoardDeviceAlias, NewState).

enable_disable_device(ok, IniFile, Board, Device, BoardDeviceAlias, NewState) ->
    {Result1, Num_devicesStr} = ini_file(IniFile, Board, "num_devices"),
    {Result2, Dependencies1} = read_field_from_cfg_anyway(IniFile, Board, Device, "dependencies"),
    enable_disable_device({Result1, Num_devicesStr}, {Result2, Dependencies1}, IniFile, Board, Device, BoardDeviceAlias, NewState);
enable_disable_device(_SupportedDevice, _IniFile, _Board, _Device, _BoardDeviceAlias, _NewState) ->
    error.

enable_disable_device({ok, Num_devicesStr}, {ok, Dependencies1}, IniFile, Board, Device, BoardDeviceAlias, NewState) ->
    Device_dependent = updater_hw_devices_utils:extract_device(Dependencies1),
    Alias = updater_hw_devices_utils:extract_alias (BoardDeviceAlias),
    {Num_devices, _} = string:to_integer(Num_devicesStr),
    disable_device(IniFile, Board, Device, Device_dependent, 1, Num_devices),
    enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, 1, Num_devices);
enable_disable_device({_Result1, _Num_devicesStr}, {_Result2, _Dependencies1}, _IniFile, _Board, _Device, _BoardDeviceAlias, _NewState) ->
    error.

disable_device(_IniFile, _Board, _Device, _Device_dependent, Index, MaxDevices) when (Index > MaxDevices) ->
    ok;
disable_device(IniFile, Board, Device, Device_dependent, Index, MaxDevices) ->
    {Result, Device1} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    disable_device({Result, Device1}, IniFile, Board, Device, Device_dependent, Index, MaxDevices).

disable_device({ok, Device1}, IniFile, Board, Device, Device_dependent, Index, MaxDevices) when
    ((Device1 == Device) or (Device1 == Device_dependent)) ->
        ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), "0", wr),
        disable_device(IniFile, Board, Device, Device_dependent, Index + 1, MaxDevices);
disable_device({_Result, _Device1}, IniFile, Board, Device, Device_dependent, Index, MaxDevices) ->
    disable_device(IniFile, Board, Device, Device_dependent, Index + 1, MaxDevices).

enable_device(_IniFile, _Board, _Device, _Device_dependent, _Alias, _NewState, Index, MaxDevices) when (Index > MaxDevices) ->
    ok;
enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) ->
    {Result1, Device1} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Alias1} = ini_file(IniFile, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
    enable_device({Result1, Device1}, {Result2, Alias1},IniFile, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices).

enable_device({ok, Device1}, {ok, Alias1},IniFile, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) when
    ((Device1 == Device) or (Device1 == Device_dependent)) and (Alias == Alias1) ->
        ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), NewState, wr),
        enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices);
enable_device({_Result1, _Device1}, {_Result2, _Alias1},IniFile, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) ->
    enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices).



%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec show_devices(string, string, string) -> {result, string}.
show_devices(IniFile, BaseBoard, Active) when (IniFile == ""); (BaseBoard == ""); (Active == "") ->
    io:format("Warning: No board was identified!~n");
show_devices(IniFile, BaseBoard, Active) ->
    {Result, NumBoardsStr} = ini_file(IniFile, "boards", "num_boards"),
    show_devices({Result, NumBoardsStr}, IniFile, BaseBoard, Active).
show_devices({ok, NumBoardsStr}, IniFile, BaseBoard, Active) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    show_devices_next_board(IniFile, BaseBoard, Active, 1, NumBoards);
show_devices({_Result, _NumBoardsStr}, _IniFile, _BaseBoard, _Active) ->
    error.

show_devices_next_board(_IniFile, _BaseBoard, _Active, Index, MaxBoards) when (Index > MaxBoards) ->
    ok;
show_devices_next_board(IniFile, BaseBoard, Active, Index, MaxBoards) ->
    {Result, Board} = ini_file(IniFile, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    show_devices_next_board({Result, Board}, IniFile, BaseBoard, Active, Index, MaxBoards).
show_devices_next_board({ok, Board}, IniFile, BaseBoard, Active, Index, MaxBoards) ->
    {_, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    show_devices_next_Device(IniFile, Board, BaseBoard, Active, 1, NumDevices),
    show_devices_next_board(IniFile, BaseBoard, Active, Index + 1, MaxBoards);
show_devices_next_board({_Result, _Board}, _IniFile, _BaseBoard, _Active, _Index, _MaxBoards) ->
    error.

show_devices_next_Device(_IniFile, _Board, _BaseBoard, _Active, Index, MaxDevices) when (Index > MaxDevices) ->
    ok;
show_devices_next_Device(IniFile, Board, BaseBoard, Active, Index, MaxDevices) ->
    {Result1, Device} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Activecard} = ini_file(IniFile, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
    {Result3, Enabled} = ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    show_devices_next_Device({Result1, Device}, {Result2, Activecard}, {Result3, Enabled}, IniFile, Board, BaseBoard, Active, Index, MaxDevices).
show_devices_next_Device({ok, Device}, {ok, Activecard}, {ok, Enabled}, IniFile, Board, BaseBoard, Active, Index, MaxDevices) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") ->
    io:format("                ~s_~s~n", [Board, Device]),
    show_devices_next_Device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices);
show_devices_next_Device({_Result1, _Device}, {_Result2, _Activecard}, {_Result3, _Enabled}, IniFile, Board, BaseBoard, Active, Index, MaxDevices) ->
    show_devices_next_Device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec show_boards_tree(string, string, string) -> ok | error.
show_boards_tree(IniFile, BaseBoard, Active) ->
    {Result, NumBoardsStr} = ini_file(IniFile, "boards", "num_boards"),
    show_boards_tree({Result, NumBoardsStr}, IniFile, BaseBoard, Active).
show_boards_tree({ok, NumBoardsStr}, IniFile, BaseBoard, Active) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    show_boards_tree_next_board(IniFile, BaseBoard, Active, 1, NumBoards);
show_boards_tree({_Result, _NumBoardsStr}, _IniFile, _BaseBoard, _Active) ->
    error.

show_boards_tree_next_board(_IniFile, _BaseBoard, _Active, Index, MaxBoards) when (Index >MaxBoards) ->
    ok;
show_boards_tree_next_board(IniFile, BaseBoard, Active, Index, MaxBoards) ->
    {Result, Board} = ini_file(IniFile, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    show_boards_tree_next_board({Result, Board}, IniFile, BaseBoard, Active, Index, MaxBoards).
show_boards_tree_next_board({ok, Board}, IniFile, BaseBoard, Active, Index, MaxBoards) ->
    {_, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, 1, NumDevices, 0),
    show_boards_tree_next_board(IniFile, BaseBoard, Active, Index + 1, MaxBoards);
show_boards_tree_next_board({_Result, _Board}, _IniFile, _BaseBoard, _Active, _Index, _MaxBoards) ->
    error.


show_boards_tree_next_device(_IniFile, _Board, _BaseBoard, _Active, Index, MaxDevices, _Flag) when (Index > MaxDevices) ->
    ok;
show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag) ->
     {Result1, Device} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
     {Result2, Activecard} = ini_file(IniFile, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
     {Result3, Enabled} = ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
     {Result4, Alias} = ini_file(IniFile, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
     show_boards_tree_next_device({Result1, Device}, {Result2, Activecard}, {Result3, Enabled}, {Result4, Alias}, IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag).
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias == "") and (Flag == 0) ->
    io:format("~n"),
    io:format("             ~s~n", [Board]),
    io:format("              +-- ~s~n", [Device]),
    show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias == "") and (Flag /= 0) ->
    io:format("              +-- ~s~n", [Device]),
    show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias /= "") and (Flag == 0) ->
    io:format("~n"),
    io:format("             ~s~n", [Board]),
    io:format("              +-- ~s (~s)~n", [Device, Alias]),
    show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({ok, Device}, {ok, Activecard}, {ok, Enabled}, {ok, Alias}, IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag) when
    ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and (Enabled == "1") and (Alias /= "") and (Flag /= 0) ->
    io:format("              +-- ~s (~s)~n", [Device, Alias]),
    show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);
show_boards_tree_next_device({_Result1, _Device}, {_Result2, _Activecard}, {_Result3, _Enabled}, {_Result4, _Alias}, IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag) ->
    show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag).


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec check_for_supported_board(string, string) -> ok | error | not_found.
check_for_supported_board(IniFile, Board) ->
    check_for_supported_board(IniFile, ini_file(IniFile, "boards", "num_boards"), Board).

check_for_supported_board(IniFile, {ok, NumBoardsStr}, Board) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    check_for_supported_board(IniFile, Board, 1, NumBoards);
check_for_supported_board(_IniFile, {error,_}, _Board) ->
    error.

check_for_supported_board(_IniFile, _Board, Index, MaxBoards) when Index > MaxBoards ->
    not_found;
check_for_supported_board(IniFile, Board, Index, MaxBoards) ->
    {Result, BoardRead} = ini_file(IniFile, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    check_for_supported_board_next(IniFile, {Result, BoardRead}, Board, Index, MaxBoards).

check_for_supported_board_next(_IniFile, {ok, Board}, Board, _Index, _MaxBoards) ->
    ok;
check_for_supported_board_next(_IniFile, {error, _BoardRead}, _Board, _Index, _MaxBoards) ->
    error;
check_for_supported_board_next(IniFile, _Result, Board, Index, MaxBoards) ->
    check_for_supported_board(IniFile, Board, Index + 1, MaxBoards).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec update_cfg_file(string, string) -> {result, string}.
update_cfg_file(Source, Target) ->
    {Result, NumBoardsStr} = ini_file(Source, "boards", "num_boards"),
    update_cfg_file({Result, NumBoardsStr}, Source, Target).
update_cfg_file({ok, NumBoardsStr}, Source, Target) ->
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    update_cfg_file_next_board(Source, Target, 1, NumBoards);
update_cfg_file({_, _NumBoardsStr}, _Source, _Target) ->
    error.

update_cfg_file_next_board(_Source, _Target, Index, MaxBoards) when (Index > MaxBoards) ->
    ok;
update_cfg_file_next_board(Source, Target, Index, MaxBoards) ->
    {Result, BoardRead} = ini_file(Source, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    update_cfg_file_next_board({Result, BoardRead}, Source, Target, Index, MaxBoards).
update_cfg_file_next_board({ok, BoardRead}, Source, Target, Index, MaxBoards) ->
    {_, NumDevicesStr} = ini_file(Source, BoardRead, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    update_cfg_file_next_Device(Source, Target, BoardRead, 1, NumDevices),
    update_cfg_file_next_board(Source, Target, Index + 1, MaxBoards);
update_cfg_file_next_board({_, _BoardRead}, _Source, _Target, _Index, _MaxBoards) ->
    error.

update_cfg_file_next_Device(_Source, _Target, _BoardRead, Index, MaxDevices) when (Index > MaxDevices) ->
    ok;
update_cfg_file_next_Device(Source, Target, BoardRead, Index, MaxDevices) ->
    {Result1, Device} = ini_file(Source, BoardRead, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Enabled} = ini_file(Source, BoardRead, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    {Result3, Alias} = ini_file(Source, BoardRead, unicode:characters_to_list(["alias", integer_to_list(Index)])),
    update_cfg_file_next_Device({Result1, Device}, {Result2, Enabled}, {Result3, Alias}, Source, Target, BoardRead, Index, MaxDevices).
update_cfg_file_next_Device({ok, Device}, {ok, Enabled}, {ok, Alias}, Source, Target, BoardRead, Index, MaxDevices) ->
    {_, IndexTarget} = get_device_index_from_cfg(Target, BoardRead, Device, "alias", Alias),
    ini_file(Target, BoardRead, unicode:characters_to_list(["enabled", integer_to_list(IndexTarget)]), Enabled, wr),
    update_cfg_file_next_Device(Source, Target, BoardRead, Index + 1, MaxDevices);
update_cfg_file_next_Device({_Result1, _Device}, {_Result2, _Enabled}, {_Result3, _Alias}, _Source, _Target, _BoardRead, _Index, _MaxDevices) ->
    error.
