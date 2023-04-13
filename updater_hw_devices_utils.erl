-module(updater_hw_devices_utils).
-include("updater_hw_devices_defines.hrl").

% Already tested and working
-export([bubble_sort/1,
         procura_string/2,
         procura_string_arquivo/2,
         find_character/2,
         extract_board_device/1,
         extract_device/1,
         extract_board/1,
         extract_alias/1,
         extract_substring/3,
         concatena_strings/1,
         list_replace/3,
         list_insert/3,
         ini_file/3,
         ini_file/5,
         list_delete/2,
         remove_char/2,
         read_field_from_cfg/4,
         read_field_from_cfg_anyway/4,
         get_device_index_from_cfg/5,
         get_file_image_name/4,
         md5_check/2,
         binary_to_hex/1,
         check_for_files_dependencies/2,
         check_file_exists/1,
         check_for_supported_devices/5,
         enable_disable_device/5,
         show_devices/3,
         show_boards_tree/3,
         platforms_list/0,
         check_for_supported_platform/1,
         check_for_supported_board/2,
         update_cfg_file/2,
         dec2ascii/1,
         hex2dec/1,
         dec2hex/1,
         bin2hex/1,
         split_paramenter/2,
         extract_platform/2,
         remove_substring/2,
         boards_list/0]).


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
   
%-record(person,
%            {
%                name,
%                age,
%                address
%            }).

%-record(state, {
%    slot_id :: non_neg_integer(),
%    type :: atom()
%}).
%
%-type state() :: #state{}.
%
%-type interface_token() :: #{
%    chassis := integer(),
%    options := [],
%    port := integer(),
%    protocol := string(),
%    slot := integer(),
%    subport := undefined | non_neg_integer() | [pos_integer()],
%    subslot := integer(),
%    type := iface
%}.

%Iface = #{
%        type => iface,
%        protocol => string:lowercase(Prot),
%        chassis => l2i(C),
%        slot => l2i(S),
%        subslot => l2i(SS),
%        port => Port,
%        subport => SubPort,
%        options => []
%    },

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
%
% bubble_sort
%
bubble_sort(List) ->
    bubble_sort(List, length(List)).

bubble_sort(List, N) when N > 0 ->
    SortedList = bubble(List, N),
    bubble_sort(SortedList, N-1);
bubble_sort(List, _) ->
    List.

bubble([A, B | T], N) when A < B ->
    [B, A | bubble(T, N)];
bubble([H | T], N) ->
    [H | bubble(T, N)];
bubble([], _) ->
    [].


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
% função que procura uma string em um arquivo
procura_string(NomeArquivo, StringProcurada) ->
    {ok, Arquivo} = file:open(NomeArquivo, [read]),
    case procura_string_arquivo(Arquivo, StringProcurada) of
        true ->
            file:close(Arquivo),
            {ok, "String encontrada com sucesso"};
        false ->
            file:close(Arquivo),
            {error, "String não encontrada"}
    end.

% função auxiliar que faz a busca pela string no arquivo
procura_string_arquivo(Arquivo, StringProcurada) ->
    case io:get_line(Arquivo, "") of
        eof ->
            false;
        Linha ->
            case string:str(Linha, StringProcurada) of
                0 ->
                    procura_string_arquivo(Arquivo, StringProcurada);
                _ ->
                    true
            end
    end.



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
        

%_____________________________________________________________________________________________
%
%                         Utils for updater_hw_devices
%_____________________________________________________________________________________________


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
split_paramenter(Parameter, Char) ->
    Index = string:str(Parameter, Char),

    case Index of
        _ when (Index > 0 ) ->
            Par1 = string:slice(Parameter, 0, Index-1),
            Par2 = string:slice(Parameter, Index, abs(length(Parameter)-Index)),
            {Par1, Par2};
     
        _ ->
           {Parameter, []}
    end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_board_device(BoardDevice) ->
    Index1 = string:str(BoardDevice, "_"),
    Index2 = string:str(BoardDevice, "/"),

    case Index1 of
        _ when (Index1 > 0 ) and (Index2 > 0) ->
            SizeBoardDevice = string:length(BoardDevice)-Index2,
            SizeAlias = string:length(BoardDevice)-SizeBoardDevice,
            Board = string:slice(BoardDevice, 0, Index1-1),
            Device = string:slice(BoardDevice, Index1, abs(Index2-Index1-1)),
            Alias = string:slice(BoardDevice, Index2, SizeAlias),
            {ok, Board, Device, Alias};

        _ when (Index1 > 0 ) and (Index2 == 0) ->
            Board = string:slice(BoardDevice, 0, Index1-1),
            Device = string:slice(BoardDevice, Index1, abs(string:length(BoardDevice)-Index1)),
            {ok, Board, Device, []};
     
        _ ->
           {error, [], [], []}
    end.
 

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_device(BoardDevice) ->
    {Status, _, Device, _} = extract_board_device(BoardDevice),
    
    case Status of
        ok ->
            Device;
        error ->
            ""
    end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_board(BoardDevice) ->
    {Status, Board, _, _} = extract_board_device(BoardDevice),
    
    case Status of
        ok ->
            Board;
        error ->
            ""
    end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_alias(BoardDevice) ->
    {Status, _, _, Alias} = extract_board_device(BoardDevice),


    case Status of
        ok ->
            Alias;
        error ->
            ""
    end.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec extract_platform(string, string) -> {result, string}.
extract_platform(Input, Prefix) ->
    Index = string:str(Input, Prefix),

    case Index of 
        _ when (Index > 0 ) ->
            Platform = string:slice(Input, abs(Index + length(Prefix) - 1), 5),
            {ok, Platform};
     
        _ ->
            {error, []}
    end.

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

    case Index of
        1 -> Tail;
        _ -> [Head | list_delete(Tail, Index-1)]
    end.

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
    case filelib:is_file(File) of
        true ->
            {ok, File};

        false ->
            {error, "file not found"}
    end.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec md5_check(string, string) -> {result, string}.
md5_check(FileName, ExpectedMD5Sum) ->
    
    {Result, Binary} = file:read_file(FileName),

    case Result of
        ok ->
            Md5SumBinary = crypto:hash(md5, Binary),
            Md5SumHex = string:lowercase(binary_to_hex(Md5SumBinary)),

            if
                (Md5SumHex == ExpectedMD5Sum) ->
                    {ok, Md5SumHex};

                true ->
                    {error, Md5SumHex}
            end;

        _ ->
            {error, "file not found"}
    end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_field(Field) ->
    Index1 = string:str(Field, " = "),
    
    case Index1 of 
        _ when (Index1 > 0 ) ->
            FieldSize = string:length(Field)-Index1,
            FieldValue = string:slice(Field, Index1+2, abs(FieldSize-2)),
            FieldValue;
        _ ->
           error
    end.


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

    case Status of
        _ when (Status == ok) ->
            List = string:tokens(binary_to_list(FileData), "\n"),
            {Result, NextIndex} = ini_file_search_for_sector(List, 1, Sector),

            case Result of
                true ->
                    ini_file_search_for_field(IniFile, List, NextIndex + 1, Field, NewFieldValue, Oper);
                    
                false ->
                    case Oper of
                        wr ->
                            NewSector = unicode:characters_to_list(["[", Sector, "]"]),
                            NewField = unicode:characters_to_list([Field, " = ", NewFieldValue]),
                            List1 = lists:append(List, [NewSector]),
                            List2 = lists:append(List1, [NewField]),
                            Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List2]),
                            file:write_file(IniFile, Text),
                            {ok, "New Sector and field added at end of file"};

                        rd ->
                            {error, "Sector not found"}
                    end
            end;

        _ ->
            {error, "Fail to access the configuration file"}
            
    end.

-spec ini_file_search_for_sector([list], number, string) -> {status, number}.
ini_file_search_for_sector(List, Index, Sector) when (Index < length(List)), (Index > 0) ->
    CurrentSector = lists:nth(Index, List),

    case string:str(CurrentSector, unicode:characters_to_list(["[", Sector, "]"])) of
        0 ->
            ini_file_search_for_sector(List, Index + 1, Sector);
        _ ->
            {true, Index}
    end;

ini_file_search_for_sector(_, _, _) ->
    {false, -1}.

ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper) when (Index > length(List)); (Index < 0)->
    ini_file_add_new_field(IniFile, List, Index, Field, NewFieldValue, Oper);

ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper) ->
    CurrentField = lists:nth(Index, List),
    EndOfSector = string:str(CurrentField, "["),

    case EndOfSector of 
        _ when (EndOfSector > 0) ->
            ini_file_add_new_field(IniFile, List, Index, Field, NewFieldValue, Oper);

        _ ->
            case string:str(CurrentField, unicode:characters_to_list([Field, " = "])) of
                0 ->
                    ini_file_search_for_field(IniFile, List, Index + 1, Field, NewFieldValue, Oper);
                _ ->
                    CurrentFieldValue = extract_field(CurrentField),
                    ini_file_replace_field(IniFile, List, Index, Field, CurrentFieldValue, NewFieldValue, Oper) 
            end
    end.

ini_file_add_new_field(IniFile, List, Index, Field, FieldValue, Oper) when (Oper == wr) ->
     case Index of 
         _ when (Index > length(List)) ->
             NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
             List1 = lists:append(List, [NewField]),
             Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
             Result = file:write_file(IniFile, Text),
             {Result, "Creating new field at end of file"};

         _ ->
             NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
             List1 = list_insert(List, Index, NewField),
             Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
             Result = file:write_file(IniFile, Text),
             {Result, "Creating new field at end of sector"}
     end;

ini_file_add_new_field(_, _, _, _, _, _) ->
    {error, "Field not found"}.

ini_file_replace_field(IniFile, List, Index, Field, _, NewFieldValue, Oper) when (Oper == wr) ->
    NewField = unicode:characters_to_list([Field, " = ", NewFieldValue]),
    List1 = list_replace(List, Index, NewField),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(IniFile, Text),
    {Result, "Updating the field"};

ini_file_replace_field(_, _, _, _, CurrentFieldValue, _, _) ->
    {ok, CurrentFieldValue}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_field_from_cfg(string, string, string, string) -> {result, string}.
read_field_from_cfg(IniFile, Board, Device, Field) ->
    {Result, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            read_field_from_cfg_search_for_device(IniFile, Board, Device, Field, 1, NumDevices);

        _ ->
            {error, NumDevicesStr}
    end.

read_field_from_cfg_search_for_device(IniFile, Board, Device, Field, Index, MaxDevices) when (Index =< MaxDevices) ->
    {Result1, DevicesStr} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, EnabledStr} = ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),

    case {Result1, Result2, DevicesStr, EnabledStr} of
         {ok, ok, Device, "1"}  ->
             ini_file(IniFile, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));

        _ ->
            read_field_from_cfg_search_for_device(IniFile, Board, Device, Field, Index + 1, MaxDevices)
    end;

read_field_from_cfg_search_for_device(_, _, _, _, _, _) ->
    {error, "field not found"}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_field_from_cfg_anyway(string, string, string, string) -> {result, string}.
read_field_from_cfg_anyway(IniFile, Board, Device, Field) ->
    {Result, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            read_field_from_cfg_anyway_search_for_device(IniFile, Board, Device, Field, 1, NumDevices);

        _ ->
            {error, NumDevicesStr}
    end.

read_field_from_cfg_anyway_search_for_device(IniFile, Board, Device, Field, Index, MaxDevices) when (Index =< MaxDevices) ->
    {Result1, DevicesStr} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),

    case {Result1, DevicesStr} of
         {ok, Device} ->
            ini_file(IniFile, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));

        _ ->
            read_field_from_cfg_anyway_search_for_device(IniFile, Board, Device, Field, Index + 1, MaxDevices)
    end;


read_field_from_cfg_anyway_search_for_device(_, _, _, _, _, _) ->
    {error, "field not found"}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec get_device_index_from_cfg(string, string, string, string, string) -> {result, number}.
get_device_index_from_cfg(IniFileName, Board, Device, Field, Value) ->
    {Result, NumDevicesStr} = ini_file(IniFileName, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, 1, NumDevices);

        _ ->
            {error, -1}
    end.

get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, Index, MaxDevices) when (Index =< MaxDevices) ->
    {Result1, DevicesValueStr} = ini_file(IniFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, FieldValueStr} = ini_file(IniFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)])),
    
    case {Result1, Result2, DevicesValueStr, FieldValueStr} of
         {ok, ok, Device, Value} ->
            {ok, Index};

        _ ->
            get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, Index + 1, MaxDevices)
    end;

get_device_index_from_cfg_search_for_device(_, _, _, _, _, _, _) ->
    {error, -1}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec get_file_image_name(string, string, string, string) -> {result, string}.
get_file_image_name(IniFile, Board, Device, InputFile) when (InputFile == "") ->
    {Result1, File} = read_field_from_cfg(IniFile, Board, Device, "file"),
    {Result2, Md5} = read_field_from_cfg(IniFile, Board, Device, "md5"),

    case {Result1,Result2} of
         {ok,ok} ->
                {Result3, Msg} = md5_check(File, Md5),

                case Result3 of
                    ok ->
                        {ok, File};

                    _ ->
                        {error, Msg}
                end;

        _ ->
            {error, "can not read ini cfg file"}

    end;

get_file_image_name(_, _, _, InputFile) ->
    check_file_exists(InputFile).

%-----------------------------------------------------------------------------
%
%
%   io:format("~p ~p ~p ~n", [DevicesStr, EnabledStr, CheckversionStr]),
%
%-----------------------------------------------------------------------------
-spec check_for_supported_devices(string, string, string, string, string) -> {result, string}.
check_for_supported_devices(IniFile, Board, Device, BaseBoard, Active) ->
    {Result, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            check_for_supported_devices_search_for_device(IniFile, Board, Device, BaseBoard, Active, 1, NumDevices);

        _ ->
            error
    end.

check_for_supported_devices_search_for_device(IniFile, Board, Device, BaseBoard, Active, Index, MaxDevices) when (Index =< MaxDevices) ->
    {Result1, DevicesStr} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, ActivecardStr} = ini_file(IniFile, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),

    case {Result1,Result2} of
         {ok,ok} when ((BaseBoard == Board) or (ActivecardStr == Active)) and
                      (DevicesStr == Device) ->
                             ok;

        _ ->
            check_for_supported_devices_search_for_device(IniFile, Board, Device, BaseBoard, Active, Index + 1, MaxDevices)
    end;

check_for_supported_devices_search_for_device(_, _, _, _, _, _, _) ->
    error.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec enable_disable_device(string, string, string, string, string) -> ok | error.
enable_disable_device(IniFile, BoardDeviceAlias, NewState, BaseBoard, Active) ->
    Board = extract_board(BoardDeviceAlias),
    Device = extract_device (BoardDeviceAlias),
    Alias = extract_alias (BoardDeviceAlias),

    case check_for_supported_devices(IniFile, Board, Device, BaseBoard, Active) of
        ok ->
            {Result1, Num_devicesStr} = ini_file(IniFile, Board, "num_devices"),
            {Result2, Dependencies1} = read_field_from_cfg_anyway(IniFile, Board, Device, "dependencies"),
            Device_dependent = extract_device(Dependencies1),
            {Num_devices, _} = string:to_integer(Num_devicesStr),

            if
                (Result1 == ok) and (Result2 == ok) ->
                    disable_device(IniFile, Board, Device, Device_dependent, 1, Num_devices),
                    enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, 1, Num_devices);
                
                true ->
                    error
            end;

        _ ->
            error
    end.

disable_device(IniFile, Board, Device, Device_dependent, Index, MaxDevices) when (Index =< MaxDevices) ->
    {Result1, Device1} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),

    case Result1 of
        ok when ((Device1 == Device) or (Device1 == Device_dependent)) ->
                    ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), "0", wr),
                    disable_device(IniFile, Board, Device, Device_dependent, Index + 1, MaxDevices);

        _ ->
            disable_device(IniFile, Board, Device, Device_dependent, Index + 1, MaxDevices)
    end;

disable_device(_, _, _, _, _, _) ->
        ok.

enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) when (Index =< MaxDevices) ->
    {Result1, Device1} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Alias1} = ini_file(IniFile, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),

    case {Result1,Result2} of
          {ok,ok} when ((Device1 == Device) or (Device1 == Device_dependent)) and
                        (Alias == Alias1) ->
                            ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), NewState, wr),
                            enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices);

        _ ->
            enable_device(IniFile, Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices)
    end;

enable_device(_, _, _, _, _, _, _, _) ->
    ok.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec show_devices(string, string, string) -> {result, string}.
show_devices(IniFile, BaseBoard, Active) when (IniFile /= ""), (BaseBoard /= ""), (Active /= "") ->
    {Result1, NumBoardsStr} = ini_file(IniFile, "boards", "num_boards"),

    case Result1 of
        ok ->
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            show_devices_next_board(IniFile, BaseBoard, Active, 1, NumBoards);

        _ ->
            error
    end;

show_devices(_,_,_) ->
    io:format("Warning: No board was identified!~n").

show_devices_next_board(IniFile, BaseBoard, Active, Index, MaxBoards) when (Index =< MaxBoards) ->
    {Result1, Board} = ini_file(IniFile, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

    case Result1 of
        ok ->
            {_, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            show_devices_next_Device(IniFile, Board, BaseBoard, Active, 1, NumDevices),
            show_devices_next_board(IniFile, BaseBoard, Active, Index + 1, MaxBoards);

        _ ->
            error
    end;

show_devices_next_board(_, _, _, _, _) ->
    ok.

show_devices_next_Device(IniFile, Board, BaseBoard, Active, Index, MaxDevices) when (Index =< MaxDevices) ->
    {Result1, Device} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
    {Result2, Activecard} = ini_file(IniFile, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
    {Result3, Enabled} = ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
    

    case {Result1, Result2, Result3} of
         {ok, ok, ok} when ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                           (Enabled == "1") ->
                                io:format("                ~s_~s~n", [Board, Device]),
                                show_devices_next_Device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices);

        _ ->
            show_devices_next_Device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices)
    end;

show_devices_next_Device(_, _, _, _, _, _) ->
    ok.


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec show_boards_tree(string, string, string) -> ok | error.
show_boards_tree(IniFile, BaseBoard, Active) ->
    {Result1, NumBoardsStr} = ini_file(IniFile, "boards", "num_boards"),

    case Result1 of
        ok ->
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            show_boards_tree_next_board(IniFile, BaseBoard, Active, 1, NumBoards);

        _ ->
            error
    end.

show_boards_tree_next_board(IniFile, BaseBoard, Active, Index, MaxBoards) when (Index =< MaxBoards) ->
    {Result1, Board} = ini_file(IniFile, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

    case Result1 of 
        ok ->
            {_, NumDevicesStr} = ini_file(IniFile, Board, "num_devices"),
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, 1, NumDevices, 0),
            show_boards_tree_next_board(IniFile, BaseBoard, Active, Index + 1, MaxBoards);

        _ ->
            error
    end;

show_boards_tree_next_board(_, _, _, _, _) ->
    ok.

show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index, MaxDevices, Flag) when (Index =< MaxDevices) ->
     {Result1, Device} = ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
     {Result2, Activecard} = ini_file(IniFile, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
     {Result3, Enabled} = ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
     {Result4, Alias} = ini_file(IniFile, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),

     case {Result1, Result2 , Result3, Result4} of
            {ok, ok, ok, ok} when ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                                   (Enabled == "1") and (Alias == "") and
                                   (Flag == 0) ->
                                       io:format("~n"),
                                       io:format("             ~s~n", [Board]),
                                       io:format("              +-- ~s~n", [Device]),
                                       show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);

            {ok, ok, ok, ok} when ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                                    (Enabled == "1") and (Alias == "") and
                                    (Flag /= 0) ->
                                        io:format("              +-- ~s~n", [Device]),
                                        show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);

            {ok, ok, ok, ok} when  ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                                    (Enabled == "1") and (Alias /= "") and
                                    (Flag == 0) ->
                                        io:format("~n"),
                                        io:format("             ~s~n", [Board]),
                                        io:format("              +-- ~s (~s)~n", [Device, Alias]),
                                        show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);

            {ok, ok, ok, ok} when ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                                    (Enabled == "1") and (Alias /= "") and
                                    (Flag /= 0) ->
                                        io:format("              +-- ~s (~s)~n", [Device, Alias]),
                                        show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag + 1);

         _ ->
             show_boards_tree_next_device(IniFile, Board, BaseBoard, Active, Index + 1, MaxDevices, Flag)
     end;

show_boards_tree_next_device(_, _, _, _, _, _, _) ->
    ok.


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
-spec check_for_supported_board(string, string) -> boolean.
check_for_supported_board(IniFile, Board) ->
    {Result1, NumBoardsStr} = ini_file(IniFile, "boards", "num_boards"),

    case Result1 of
        ok ->
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            check_for_supported_board(IniFile, Board, 1, NumBoards);

        _ ->
            error
    end.

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
    {Result1, NumBoardsStr} = ini_file(Source, "boards", "num_boards"),

    case Result1 of
        ok ->
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            update_cfg_file_next_board(Source, Target, 1, NumBoards);

        _ ->
            error
    end.

update_cfg_file_next_board(Source, Target, Index, MaxBoards) when (Index =< MaxBoards), (MaxBoards > 0) ->
     {Result1, Board} = ini_file(Source, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

     case Result1 of
         ok ->
             {_, NumDevicesStr} = ini_file(Source, Board, "num_devices"),
             {NumDevices, _} = string:to_integer(NumDevicesStr),
             update_cfg_file_next_Device(Source, Target, Board, 1, NumDevices),
             update_cfg_file_next_board(Source, Target, Index + 1, MaxBoards);

         _ ->
             error
     end;

update_cfg_file_next_board(_, _, _, _) ->
    ok.

update_cfg_file_next_Device(Source, Target, Board, Index, MaxDevices) when (Index =< MaxDevices) ->
     {Result1, Device} = ini_file(Source, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
     {Result2, Enabled} = ini_file(Source, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
     {Result3, Alias} = ini_file(Source, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
     
     case {Result1, Result2, Result3} of
        {ok, ok, ok} ->
             {_, IndexTarget} = get_device_index_from_cfg(Target, Board, Device, "alias", Alias),
             ini_file(Target, Board, unicode:characters_to_list(["enabled", integer_to_list(IndexTarget)]), Enabled, wr),
             update_cfg_file_next_Device(Source, Target, Board, Index + 1, MaxDevices);

         _ ->
             error
     end;

update_cfg_file_next_Device(_, _, _, _, _) ->
    ok.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec check_for_files_dependencies(string, string) -> result.
check_for_files_dependencies(List, Index) when (Index =< length(List)), (Index > 0) ->
    Current = lists:nth(Index, List),
    {Result, _} = check_file_exists(Current),

    case Result of
        ok ->
            check_for_files_dependencies(List, Index + 1);

        error ->
            error
    end;

check_for_files_dependencies(_, _) ->
    ok.

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

    case (Index < length(BoardList)) of
        true ->
            {ok,  lists:nth(Index, BoardList)};

        false ->
            {error, "Invalid board"}
    end;

get_board_type(BoardType) ->
    {ok, BoardType}.

