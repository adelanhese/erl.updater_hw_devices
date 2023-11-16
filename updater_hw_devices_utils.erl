-module(updater_hw_devices_utils).
-include("updater_hw_devices_defines.hrl").

% Already tested and working
-export([find_character/2,
         extract_board_device/1,
         extract_device/1,
         extract_board/1,
         extract_alias/1,
         extract_substring/3,
         concatena_strings/1,
         list_replace/3,
         list_insert/3,
         list_delete/2,
         remove_char/2,
         md5_check/2,
         binary_to_hex/1,
         check_for_files_dependencies/2,
         check_file_exists/1,
         platforms_list/0,
         check_for_supported_platform/1,
         dec2ascii/1,
         hex2dec/1,
         dec2hex/1,
         bin2hex/1,
         split_paramenter/2,
         extract_platform/2,
         remove_substring/2,
         boards_list/0,
         extract_field/1]).


% ToDo
-export([fpga_reload/0,
         get_i2c_adapter/1,
         get_active/1,
         get_platform_type/1,
         get_dtb_name/1,
         get_board_type/1]).
         

platforms_list() -> [?PLAT1,
                     ?PLAT2,
                     ?PLAT6].

boards_list() -> [?NULL,
                  ?NULL,
                  ?NULL,
                  ?LC4,
                  ?SC2000,
                  ?MNGT,
                  ?LC1,
                  ?NULL,
                  ?NULL,
                  ?NULL,
                  ?LC5,
                  ?NULL].
   
%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_substring(String, Pos, Length) ->
    string:substr(Pos, Length, String).

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
find_character(Char, String) ->
            Index = string:str(String, Char),
            Index.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
split_paramenter(Parameter, Char) ->
    Index = string:str(Parameter, Char),
    split_part1_part2(Parameter, Index).

split_part1_part2(Parameter, Index) when (Index > 0 ) ->
    Part1 = string:slice(Parameter, 0, Index-1),
    Part2 = string:slice(Parameter, Index, abs(length(Parameter)-Index)),
    {Part1, Part2};
split_part1_part2(Parameter, _Index) ->
    {Parameter, []}.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_board_device(BoardDevice) ->
    Index1 = string:str(BoardDevice, "_"),
    Index2 = string:str(BoardDevice, "/"),
    extract_board_device(BoardDevice, Index1, Index2).

extract_board_device(BoardDevice, Index1, Index2) when (Index1 > 0 ) and (Index2 > 0) ->
    SizeBoardDevice = string:length(BoardDevice)-Index2,
    SizeAlias = string:length(BoardDevice)-SizeBoardDevice,
    Board = string:slice(BoardDevice, 0, Index1-1),
    Device = string:slice(BoardDevice, Index1, abs(Index2-Index1-1)),
    Alias = string:slice(BoardDevice, Index2, SizeAlias),
    {ok, Board, Device, Alias};
extract_board_device(BoardDevice, Index1, 0) when (Index1 > 0) ->
    Board = string:slice(BoardDevice, 0, Index1-1),
    Device = string:slice(BoardDevice, Index1, abs(string:length(BoardDevice)-Index1)),
    {ok, Board, Device, []};
extract_board_device(_BoardDevice, _Index1, _Index2) ->
    {error, [], [], []}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_device(BoardDevice) ->
    {Status, _, Device, _} = extract_board_device(BoardDevice),
    extract_device(Status, Device).

extract_device(ok, Device) ->
    Device;
extract_device(_Status, _Device) ->
    "".

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_board(BoardDevice) ->
    {Status, Board, _, _} = extract_board_device(BoardDevice),
    extract_board(Status, Board).

extract_board(ok, Board) ->
    Board;
extract_board(_Status, _Board) ->
    "".

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_alias(BoardDevice) ->
    {Status, _, _, Alias} = extract_board_device(BoardDevice),
    extract_alias(Status, Alias).

extract_alias(ok, Alias) ->
    Alias;
extract_alias(_Status, _Alias) ->
    error.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec extract_platform(string, string) -> {result, string}.
extract_platform(Input, Prefix) ->
    Index = string:str(Input, Prefix),
    extract_platform(Input, Prefix, Index).

extract_platform(Input, Prefix, Index) when (Index > 0) ->
    {ok, string:slice(Input, abs(Index + length(Prefix) - 1), 5)};
extract_platform(_Input, _Prefix, _Index) ->
    {error, []}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
concatena_strings(Data) ->
    unicode:characters_to_list(Data).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
list_replace(List, Index, NewValue) ->
    lists:sublist(List, Index-1) ++ [NewValue] ++ lists:nthtail(Index, List).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
list_delete(List, Index) ->
    [Head | Tail] = List,
    list_delete(Head, Tail, Index).
list_delete(_Head, Tail, 1) ->
    Tail;
list_delete(Head, Tail, Index) ->
    [Head | list_delete(Tail, Index-1)].

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
list_insert(List, Position, Element) ->
    lists:sublist(List, Position - 1) ++ [Element] ++ lists:nthtail(Position - 1, List).

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
remove_char(String, Char) ->
    %lists:filter(fun (C) -> C /= Char end, String).
    lists:flatten([C || C <- String, C /= Char]).

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
remove_substring(String, Substring) ->
   string:replace(String, Substring, "").

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec binary_to_hex(binary) -> string.
binary_to_hex(Binary) ->
    lists:flatten([io_lib:format("~2.16.0B", [Byte]) || <<Byte>> <= Binary]).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec dec2ascii(number) -> string.
dec2ascii(DecNum) ->
    integer_to_list(DecNum).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec hex2dec(string) -> number.
hex2dec(HexStr) ->
    list_to_integer(HexStr, 16).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec dec2hex(number) -> string.
dec2hex(DecNum) ->
    io_lib:format("0x~.16B", [DecNum]).


%-----------------------------------------------------------------------------
%
%  ex:
%        updater_hw_devices:bin2hex("1010").
%        "0xA"
%
%-----------------------------------------------------------------------------
-spec bin2hex(string) -> string.
bin2hex(BinStr) ->
    dec2hex(binary_to_integer(list_to_binary(BinStr), 2)).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
check_file_exists(File) ->
    CheckIfFile = filelib:is_file(File),
    check_file_exists(CheckIfFile, File).
check_file_exists(true, File) ->
    {ok, File};
check_file_exists(_CheckIfFile, _File) ->
    {error, "file not found"}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec md5_check(string, string) -> {result, string}.
md5_check(FileName, ExpectedMD5Sum) ->
    {Result, Binary} = file:read_file(FileName),
    md5_calc({Result, Binary}, ExpectedMD5Sum).

md5_calc({ok, Binary}, ExpectedMD5Sum) ->
    Md5SumBinary = crypto:hash(md5, Binary),
    Md5SumHex = string:lowercase(binary_to_hex(Md5SumBinary)),
    md5_compare(Md5SumHex, ExpectedMD5Sum);
md5_calc({_Result, _Binary}, _ExpectedMD5Sum) ->
    {error, "file not found"}.

md5_compare(Md5SumHex, ExpectedMD5Sum) when (Md5SumHex == ExpectedMD5Sum) ->
    {ok, ExpectedMD5Sum};
md5_compare(_Md5SumHex, _ExpectedMD5Sum) ->
    {error, "invalid md5"}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_field(Field) ->
    Index1 = string:str(Field, " = "),
    extract_field(Field, Index1).

extract_field(Field, Index1) when (Index1 > 0 ) ->
    FieldSize = string:length(Field)-Index1,
    FieldValue = string:slice(Field, Index1+2, abs(FieldSize-2)),
    FieldValue;
extract_field(_Field, _Index1) ->
    [].

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec check_for_supported_platform(string) -> boolean.
check_for_supported_platform(Platform) ->
    lists:member(Platform, platforms_list()).


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec check_for_files_dependencies(string, string) -> result.
check_for_files_dependencies(List, Index) when (Index > length(List)) ->
    ok;
check_for_files_dependencies(List, Index) ->
    Current = lists:nth(Index, List),
    {Result, _} = check_file_exists(Current),
    check_for_files_dependencies(Result, List, Index).
check_for_files_dependencies(ok, List, Index) ->
    check_for_files_dependencies(List, Index + 1);
check_for_files_dependencies(_Result, _List, _Index) ->
    error.

%-----------------------------------------------------------------------------
%
%                             T o D o
% 
%-----------------------------------------------------------------------------


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec fpga_reload() -> ok.
fpga_reload() ->
    ok.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_i2c_adapter(string) -> ok.
get_i2c_adapter(I2CAdapterName) ->
    io:format("~p~n", [I2CAdapterName]),
    ok.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_active(string) -> ok.
get_active(Active) when (Active == "0") ->
    % ToDo: insert here the active pin reading
    ActivePin = "0",
    {ok, ActivePin};
get_active(Active) ->
    {ok, Active}.

%-----------------------------------------------------------------------------
%
%  Whoami = "cc 6 PM_MNGT_ETSc 7 rm_etsc6 1"
%           "lc 11 PM_LC5-MP4-D 1 rm_etsc6 1"
%           "sc 5 PM_SC2000 5 rm_etsc6 1"
%
%  4 = lc4
%  5 = sc2000
%  6 = mngt
%  7 = lc1
%  11 = lc5
%-----------------------------------------------------------------------------
-spec get_platform_type(string) -> ok.
get_platform_type(PlatFormType) when (PlatFormType == "") ->
    Cmd = unicode:characters_to_list([?EHALCLI, " whoami"]),
    Whoami = remove_char(os:cmd(Cmd), 10),
    extract_platform(Whoami, " rm_");
get_platform_type(PlatFormType) ->
    {ok, PlatFormType}.

%-----------------------------------------------------------------------------
%
% /etc/dtb/device_tree_cc_etsc6.dtb
%-----------------------------------------------------------------------------
-spec get_dtb_name(string) -> ok.
get_dtb_name(DtbName) when (DtbName == "") ->
    Cmd = unicode:characters_to_list([?EHALCLI, " device-tree"]),
    Dtb_name = remove_char(os:cmd(Cmd), 10),
    {ok, Dtb_name};
get_dtb_name(DtbName) ->
    {ok, DtbName}.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_board_type(string) -> ok.
get_board_type(BoardType) when (BoardType == "") ->
    Cmd = unicode:characters_to_list([?EHALCLI, " whoami", " | awk '{ print $2 }'"]),
    Whoami = remove_char(os:cmd(Cmd), 10),
    Index = list_to_integer(Whoami, 10),
    BoardList = boards_list(),
    get_board_type(Index, BoardList);
get_board_type(BoardType) ->
    {ok, BoardType}.
get_board_type(Index, BoardList) when (Index < length(BoardList)) ->
    {ok,  lists:nth(Index, BoardList)};
get_board_type(_Index, _BoardList) ->
    {error, "Invalid board"}.


