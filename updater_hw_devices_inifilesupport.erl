-module(updater_hw_devices_inifilesupport).
-include("updater_hw_devices_defines.hrl").

% Already tested and working
-export([ini_file/3,
         ini_file/5]).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec ini_file(string, string, string) -> {status, string}.
ini_file(IniFileName, Sector, Field) ->
    ini_file(IniFileName, Sector, Field, "", rd).

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec ini_file(string, string, string, string, [rd|wr]) -> {status, string}.
ini_file(IniFileName, Sector, Field, NewFieldValue, Oper) ->
    {Status, FileData} = file:read_file(IniFileName),
    ini_file_to_list({Status, FileData}, IniFileName, Sector, Field, NewFieldValue, Oper).

ini_file_to_list({ok, FileData}, IniFileName, Sector, Field, NewFieldValue, Oper) ->
    List = string:tokens(binary_to_list(FileData), "\n"),
    {Result, NextIndex} = ini_file_search_for_sector(List, 1, Sector),
    ini_file_search_for_first_field(IniFileName, List, {Result, NextIndex}, Sector, Field, NewFieldValue, Oper);
ini_file_to_list({_Status, _FileData}, _IniFile, _Sector, _Field, _NewFieldValue, _Oper) ->
    {error, "Fail to access the configuration file"}.
    
ini_file_search_for_first_field(IniFileName, List, {true, NextIndex}, _Sector, Field, NewFieldValue, Oper) ->
    ini_file_search_for_field(IniFileName, List, NextIndex + 1, Field, NewFieldValue, Oper);
ini_file_search_for_first_field(IniFileName, List, {_Result, _NextIndex}, Sector, Field, NewFieldValue, Oper) ->
    ini_file_add_new_sector_and_field(IniFileName, List, Sector, Field, NewFieldValue, Oper).

ini_file_add_new_sector_and_field(IniFileName, List, Sector, Field, NewFieldValue, wr) ->
     NewSector = unicode:characters_to_list(["[", Sector, "]"]),
     NewField = unicode:characters_to_list([Field, " = ", NewFieldValue]),
     List1 = lists:append(List, [NewSector]),
     List2 = lists:append(List1, [NewField]),
     Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List2]),
     file:write_file(IniFileName, Text),
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
ini_file_search_for_field(IniFileName, List, Index, Field, NewFieldValue, Oper) when (Index > length(List)); (Index < 0)->
    ini_file_add_new_field(IniFileName, List, Index, Field, NewFieldValue, Oper);
ini_file_search_for_field(IniFileName, List, Index, Field, NewFieldValue, Oper) ->
    CurrentField = lists:nth(Index, List),
    EndOfSector = string:str(CurrentField, "["),
    ini_file_search_for_field(IniFileName, List, Index, Field, NewFieldValue, Oper, CurrentField, EndOfSector).

ini_file_search_for_field(IniFileName, List, Index, Field, NewFieldValue, Oper, _CurrentField, EndOfSector) when (EndOfSector > 0) ->
    ini_file_add_new_field(IniFileName, List, Index, Field, NewFieldValue, Oper);
ini_file_search_for_field(IniFileName, List, Index, Field, NewFieldValue, Oper, CurrentField, _EndOfSector) ->
    FieldSeparator = string:str(CurrentField, unicode:characters_to_list([Field, " ="])),
    ini_file_search_for_field_next(IniFileName, List, Index, Field, NewFieldValue, Oper, CurrentField, FieldSeparator).

ini_file_search_for_field_next(IniFileName, List, Index, Field, NewFieldValue, Oper, _CurrentField, 0) ->
    ini_file_search_for_field(IniFileName, List, Index + 1, Field, NewFieldValue, Oper);
ini_file_search_for_field_next(IniFileName, List, Index, Field, NewFieldValue, Oper, CurrentField, _FieldSeparator) ->
    CurrentFieldValue = updater_hw_devices_utils:extract_field(CurrentField),
    ini_file_replace_field(IniFileName, List, Index, Field, CurrentFieldValue, NewFieldValue, Oper).

%--------------------------------------
ini_file_add_new_field(IniFileName, List, Index, Field, FieldValue, Oper) when (Oper == wr) ->
    ini_file_add_new_field(IniFileName, List, Index, Field, FieldValue);
ini_file_add_new_field(_IniFile, _List, _Index, _Field, _FieldValue, _Oper) ->
    {error, "Field not found"}.

ini_file_add_new_field(IniFileName, List, Index, Field, FieldValue) when (Index > length(List)) ->
    NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
    List1 = lists:append(List, [NewField]),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(IniFileName, Text),
    {Result, "Creating new field at end of file"};
ini_file_add_new_field(IniFileName, List, Index, Field, FieldValue) ->
    NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
    List1 = updater_hw_devices_utils:list_insert(List, Index, NewField),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(IniFileName, Text),
    {Result, "Creating new field at end of sector"}.

%--------------------------------------
ini_file_replace_field(IniFileName, List, Index, Field, _CurrentFieldValue, NewFieldValue, Oper) when (Oper == wr) ->
    NewField = unicode:characters_to_list([Field, " = ", NewFieldValue]),
    List1 = updater_hw_devices_utils:list_replace(List, Index, NewField),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(IniFileName, Text),
    {Result, "Updating the field"};
ini_file_replace_field(_IniFile, _List, _Index, _Field, CurrentFieldValue, _NewFieldValue, _Oper) ->
    {ok, CurrentFieldValue}.
