%
%
%   git push -u origin main
%
-module(updater_hw_devices).
-include("updater_hw_devices_defines.hrl").

-export([bubble_sort/1]).
-export([procura_string/2]).
-export([procura_string_arquivo/2]).
-export([find_character/2]).
-export([extract_board_device/1]).
-export([extract_device/1]).
-export([extract_board/1]).
-export([extract_alias/1]).
-export([extract_substring/1]).
-export([concatena_strings/1]).
-export([list_replace/3]).
-export([list_insert/3]).
-export([ini_file/3]).
-export([ini_file/5]).
-export([list_delete/2]).
-export([remove_char/2]).
-export([read_field_from_cfg/3]).
-export([read_field_from_cfg_anyway/3]).
-export([get_device_index_from_cfg/5]).
-export([get_file_image_name/3]).
-export([md5_check/2]).
-export([binary_to_hex/1]).
-export([dependencies_list_etsc1/0]).
-export([dependencies_list_etsc2/0]).
-export([dependencies_list_etsc6/0]).
-export([check_for_dependencies/1]).
-export([check_file_exists/1]).
-export([check_for_supported_devices/4]).
-export([enable_disable_device/4]).
-export([show_help/0]).
-export([show_devices/2]).
-export([show_boards_tree/2]).
-export([platforms_list/0]).
-export([check_for_supported_platform/1]).
-export([check_for_supported_board/1]).
-export([update_cfg_file/2]).
-export([dec2ascii/1]).
-export([hex2dec/1]).
-export([dec2hex/1]).
-export([bin2hex/1]).


% ToDo
-export([fpga_reload/0]).
-export([get_i2c_adapter/1]).
-export([get_platform_type/0]).
-export([get_dtb_name/0]).
-export([get_board_type/0]).


platforms_list() -> [?ETSC1,
                     ?ETSC2,
                     ?ETSC6].

dependencies_list_test() -> [?NOHUP,
                        ?NOTIFY_SEND,
                        ?NPROC,
                        ?NROFF,
                        ?NSENTER,
                        ?NSLOOKUP,
                        ?NSS_ADDBUILTIN,
                        ?NSS_DBTEST,
                        ?NSS_PP,
                        ?NSTAT,
                        ?NSUPDATE,
                        ?NTFS_3G,
                        ?NTFS_3G_PROBE,
                        ?NTFSCAT,
                        ?NTFSCLUSTER,
                        ?NTFSCMP,
                        ?NTFSDECRYPT,
                        ?NTFSFALLOCATE,
                        ?NTFSFIX,
                        ?NTFSINFO,
                        ?NTFSLS,
                        ?NTFSMOVE,
                        ?NTFSRECOVER,
                        ?NTFSSECAUDIT,
                        ?NTFSTRUNCATE,
                        ?NTFSUSERMAP,
                        ?NTFSWIPE,
                        ?NUMFMT,
                        ?NVIDIA_DETECTOR,
                        ?NVLC].


dependencies_list_etsc1() -> [?FPGAIO,
                               ?I2CGET,
                               ?I2CSET,
                               ?I2CTRANSFER,
                               ?I2CDETECT,
                               ?FAN,
                               ?DD,
                               ?MD5SUM,
                               ?MACHXO2,
                               ?GPIOGET,
                               ?GPIOSET,
                               ?GPIOFIND,
                               ?BLHOST,
                               ?BTOOL,
                               ?KEXEC,
                               ?EHALCLI].

dependencies_list_etsc2() -> [?FPGAIO,
                               ?I2CGET,
                               ?I2CSET,
                               ?I2CTRANSFER,
                               ?I2CDETECT,
                               ?FAN,
                               ?DD,
                               ?MD5SUM,
                               ?MACHXO2,
                               ?GPIOGET,
                               ?GPIOSET,
                               ?GPIOFIND,
                               ?BLHOST,
                               ?BTOOL,
                               ?KEXEC,
                               ?ISSI_FLASH,
                               ?FLASHROM,
                               ?EHALCLI].

dependencies_list_etsc6() -> [?FPGAIO,
                               ?I2CGET,
                               ?I2CSET,
                               ?I2CTRANSFER,
                               ?I2CDETECT,
                               ?FAN,
                               ?DD,
                               ?MD5SUM,
                               ?MACHXO2,
                               ?GPIOGET,
                               ?GPIOSET,
                               ?GPIOFIND,
                               ?BLHOST,
                               ?BTOOL,
                               ?KEXEC,
                               ?ISSI_FLASH,
                               ?FLASHROM,
                               ?EHALCLI].


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
extract_substring(String) ->
        Substring = string:substr(3, 5, String),
        Substring.



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
extract_board_device(BoardDevice) ->
    Index1 = string:str(BoardDevice, "_"),
    Index2 = string:str(BoardDevice, "/"),

    if
        (Index1 > 0 ) ->
            SizeBoardDevice = string:length(BoardDevice)-Index2,
            SizeAlias = string:length(BoardDevice)-SizeBoardDevice,
            Board = string:slice(BoardDevice, 0, Index1-1),
            Device = string:slice(BoardDevice, Index1, abs(Index2-Index1-1)),
            Alias = string:slice(BoardDevice, Index2, SizeAlias),
            {ok, Board, Device, Alias};
        true ->
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

    case (Result == ok) of
        true ->
            Md5SumBinary = crypto:hash(md5, Binary),
            Md5SumHex = binary_to_hex(Md5SumBinary),

            if
                (Md5SumHex == ExpectedMD5Sum) ->
                    {ok, Md5SumHex};

                true ->
                    {error, Md5SumHex}
            end;

        false ->
            {error, "file not found"}
    end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
extract_field(Field) ->
    Index1 = string:str(Field, " = "),
    
    if
        (Index1 > 0 ) ->
            FieldSize = string:length(Field)-Index1,
            FieldValue = string:slice(Field, Index1+2, abs(FieldSize-2)),
            FieldValue;
        true ->
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

    if
        (Status == ok) ->
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

        true ->
            {error, "Fail to access the file"}
            
    end.

-spec ini_file_search_for_sector([list], number, string) -> {status, number}.
ini_file_search_for_sector(List, Index, Sector) ->
    if
        (Index < length(List)) and (Index > 0) ->
            CurrentSector = lists:nth(Index, List),

            case string:str(CurrentSector, unicode:characters_to_list(["[", Sector, "]"])) of
                0 ->
                    ini_file_search_for_sector(List, Index + 1, Sector);
                _ ->
                    {true, Index}
            end;

        true ->
            {false, -1}
    end.

-spec ini_file_search_for_field(string, [list], number, string, string, [rd|wr]) -> {status, string}.
ini_file_search_for_field(IniFile, List, Index, Field, NewFieldValue, Oper) ->
    if
        (Index > length(List)) or (Index < 0) ->
            ini_file_add_new_field(IniFile, List, Index, Field, NewFieldValue, Oper);

        true ->
                CurrentField = lists:nth(Index, List),
                EndOfSector = string:str(CurrentField, "["),

                if
                    (EndOfSector > 0) ->
                        ini_file_add_new_field(IniFile, List, Index, Field, NewFieldValue, Oper);

                    true ->
                        case string:str(CurrentField, unicode:characters_to_list([Field, " = "])) of
                            0 ->
                                ini_file_search_for_field(IniFile, List, Index + 1, Field, NewFieldValue, Oper);
                            _ ->
                                CurrentFieldValue = extract_field(CurrentField),
                                ini_file_replace_field(IniFile, List, Index, Field, CurrentFieldValue, NewFieldValue, Oper) 
                        end
                end
    end.

-spec ini_file_add_new_field(string, [list], number, string, string, [rd|wr]) -> {staus, string}.
ini_file_add_new_field(IniFile, List, Index, Field, FieldValue, Oper) ->
    case Oper of
        wr ->
            if
                (Index > length(List)) ->
                    NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
                    List1 = lists:append(List, [NewField]),
                    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
                    Result = file:write_file(IniFile, Text),
                    {Result, "Creating new field at end of file"};

                true ->
                    NewField = unicode:characters_to_list([Field, " = ", FieldValue]),
                    List1 = list_insert(List, Index, NewField),
                    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
                    Result = file:write_file(IniFile, Text),
                    {Result, "Creating new field at end of sector"}
            end;

        rd ->
            {error, "Field not found"}
    end.

-spec ini_file_replace_field(string, [list], number, string, string, string, [rd|wr]) -> {status, string}.
ini_file_replace_field(IniFile, List, Index, Field, CurrentFieldValue, NewFieldValue, Oper) ->
        case Oper of
            wr ->
                NewField = unicode:characters_to_list([Field, " = ", NewFieldValue]),
                List1 = list_replace(List, Index, NewField),
                Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
                Result = file:write_file(IniFile, Text),
                {Result, "Updating the field"};
   
            rd ->
                {ok, CurrentFieldValue}
        end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_field_from_cfg(string, string, string) -> {result, string}.
read_field_from_cfg(Board, Device, Field) ->
    {Result, NumDevicesStr} = ini_file(?INI_FILE, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            read_field_from_cfg_search_for_device(Board, Device, Field, 1, NumDevices);

        _ ->
            {error, NumDevicesStr}
    end.

read_field_from_cfg_search_for_device(Board, Device, Field, Index, MaxDevices) ->
    DeviceFieldStr = unicode:characters_to_list(["device", integer_to_list(Index)]),
    EnabledFieldStr = unicode:characters_to_list(["enabled", integer_to_list(Index)]),
    FieldStr = unicode:characters_to_list([Field, integer_to_list(Index)]),
    {Result1, DevicesStr} = ini_file(?INI_FILE, Board, DeviceFieldStr),
    {Result2, EnabledStr} = ini_file(?INI_FILE, Board, EnabledFieldStr),

    if
        (Index =< MaxDevices) ->
            if
                (Result1 == ok) and (Result2 == ok) and (DevicesStr == Device) and (EnabledStr == "1") ->
                    ini_file(?INI_FILE, Board, FieldStr);

                true ->
                    read_field_from_cfg_search_for_device(Board, Device, Field, Index + 1, MaxDevices)
            end;

        true ->
            {error, "field not found"}
    end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_field_from_cfg_anyway(string, string, string) -> {result, string}.
read_field_from_cfg_anyway(Board, Device, Field) ->
    {Result, NumDevicesStr} = ini_file(?INI_FILE, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            read_field_from_cfg_anyway_search_for_device(Board, Device, Field, 1, NumDevices);

        _ ->
            {error, NumDevicesStr}
    end.

read_field_from_cfg_anyway_search_for_device(Board, Device, Field, Index, MaxDevices) ->

    if
        (Index =< MaxDevices) ->
            {Result1, DevicesStr} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),

            if
                (Result1 == ok) and (DevicesStr == Device) ->
                    ini_file(?INI_FILE, Board, unicode:characters_to_list([Field, integer_to_list(Index)]));

                true ->
                    read_field_from_cfg_anyway_search_for_device(Board, Device, Field, Index + 1, MaxDevices)
            end;

        true ->
            {error, "field not found"}
    end.


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

get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, Index, MaxDevices) ->

    if
        (Index =< MaxDevices) ->
            {Result1, DevicesValueStr} = ini_file(IniFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, FieldValueStr} = ini_file(IniFileName, Board, unicode:characters_to_list([Field, integer_to_list(Index)])),
            
            if
                (Result1 == ok) and (Result2 == ok) and (DevicesValueStr == Device) and (FieldValueStr == Value) ->
                    {ok, Index};

                true ->
                    get_device_index_from_cfg_search_for_device(IniFileName, Board, Device, Field, Value, Index + 1, MaxDevices)
            end;

        true ->
            {error, -1}
    end.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec get_file_image_name(string, string, string) -> {result, string}.
get_file_image_name(Board, Device, InputFile) ->

    case (InputFile == "") of
        true ->
            {Result1, File} = read_field_from_cfg(Board, Device, "file"),
            {Result2, Md5} = read_field_from_cfg(Board, Device, "md5"),

            if
                (Result1 == ok) and (Result2 == ok) ->
                    {Result3, Msg} = md5_check(File, Md5),

                    case Result3 of
                        ok ->
                            {ok, File};

                        _ ->
                            {error, Msg}
                    end;

                true ->
                    {error, "can not read ini cfg file"}

            end;

        false ->
            check_file_exists(InputFile)
    end.



%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec check_for_dependencies(string) -> ok | error.
check_for_dependencies(Platform) ->

    case Platform of

        ?ETSC1 ->
            check_for_dependencies(dependencies_list_etsc1(), 1);

        ?ETSC2 ->
            check_for_dependencies(dependencies_list_etsc2(), 1);

        ?ETSC6 ->
            check_for_dependencies(dependencies_list_etsc6(), 1);
    
        ?TEST ->
            check_for_dependencies(dependencies_list_test(), 1);
        
         _ ->
            error

    end.

check_for_dependencies(List, Index) ->

    if
        (Index =< length(List)) and (Index > 0) ->

            Current = lists:nth(Index, List),
            {Result, _} = check_file_exists(Current),

            case Result of
                ok ->
                    check_for_dependencies(List, Index + 1);

                error ->
                    error
            end;

        true ->
            ok
    end.


%-----------------------------------------------------------------------------
%
%
%   io:format("~p ~p ~p ~n", [DevicesStr, EnabledStr, CheckversionStr]),
%
%-----------------------------------------------------------------------------
-spec check_for_supported_devices(string, string, string, string) -> {result, string}.
check_for_supported_devices(Board, Device, CurrentBoard, Active) ->
    {Result, NumDevicesStr} = ini_file(?INI_FILE, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            check_for_supported_devices_search_for_device(Board, Device, CurrentBoard, Active, 1, NumDevices);

        _ ->
            error
    end.

check_for_supported_devices_search_for_device(Board, Device, CurrentBoard, Active, Index, MaxDevices) ->

    if
        (Index =< MaxDevices) ->
            {Result1, DevicesStr} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, ActivecardStr} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),

            if
                (Result1 == ok) and (Result2 == ok)  and
                ((CurrentBoard == Board) or (ActivecardStr == Active)) and
                (DevicesStr == Device) ->
                    ok;

                true ->
                    check_for_supported_devices_search_for_device(Board, Device, CurrentBoard, Active, Index + 1, MaxDevices)
            end;

        true ->
            error
    end.



%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec enable_disable_device(string, string, string, string) -> ok | error.
enable_disable_device(BoardDeviceAlias, NewState, CurrentBoard, Active) ->
    Board = extract_board(BoardDeviceAlias),
    Device = extract_device (BoardDeviceAlias),
    Alias = extract_alias (BoardDeviceAlias),

    case check_for_supported_devices(Board, Device, CurrentBoard, Active) of
        ok ->
            {Result1, Num_devicesStr} = ini_file(?INI_FILE, Board, "num_devices"),
            {Result2, Dependencies1} = read_field_from_cfg_anyway(Board, Device, "dependencies"),
            Device_dependent = extract_device(Dependencies1),
            {Num_devices, _} = string:to_integer(Num_devicesStr),

            if
                (Result1 == ok) and (Result2 == ok) ->
                    disable_device(Board, Device, Device_dependent, 1, Num_devices),
                    enable_device(Board, Device, Device_dependent, Alias, NewState, 1, Num_devices);
                
                true ->
                    error
            end;

        _ ->
            error
    end.

disable_device(Board, Device, Device_dependent, Index, MaxDevices) ->

    if
        (Index =< MaxDevices) ->
            {Result1, Device1} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),

            if
                (Result1 == ok) and
                ((Device1 == Device) or (Device1 == Device_dependent)) ->
                    ini_file(?INI_FILE, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), "0", wr),
                    disable_device(Board, Device, Device_dependent, Index + 1, MaxDevices);

                true ->
                    disable_device(Board, Device, Device_dependent, Index + 1, MaxDevices)
            end;

        true ->
            ok
    end.


enable_device(Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) ->

    if
        (Index =< MaxDevices) ->
            {Result1, Device1} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, Alias1} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),

            if
                (Result1 == ok) and (Result2 == ok)  and
                ((Device1 == Device) or (Device1 == Device_dependent)) and
                (Alias == Alias1) ->
                    ini_file(?INI_FILE, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), NewState, wr),
                    enable_device(Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices);

                true ->
                    enable_device(Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices)
            end;

        true ->
            ok
    end.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
show_help() ->
    io:format("~s: This tool provides the user interface to check and update the hw devices.~n", [?MODULE_NAME]),
    io:format(" ~n"),
    io:format("Tool usage:~n"),
    io:format("     ~s [options...] [command <cmd_arg>]~n", [?MODULE_NAME]),
    io:format("~n"),
    io:format("Options list: ~n"),
    io:format("    -a, --auto                                = automatically update all outdated devices~n"),
    io:format("    -b, --backplane                           = force access to backplane (active card)~n"),
    io:format("    -f, --file                                = show configuration file name~n"),
    io:format("    -g, --background                          = run in background mode (RECOMMENDED!!!)~n"),
    io:format("    -s, --spk                                 = check device version from spk image~n"),
    io:format("    -v=<level>, --verbose=<level>             = set the log level to show messages (see note 1)~n"),
    io:format("    -r, --fpga-reload                         = perform system reboot and force fpga reload after update~n"),
    io:format("    -p, --power-cycle                         = perform power cycle after update~n"),
    io:format("    -l, --system-reboot                       = perform system reboot after update~n"),
    io:format("    -i=<file> or --input=<file>               = force an input file name to burn the device~n"),
    io:format("    -o=<cardid> or --cardid=<cardid>          = force a card id (see note 2)~n"),
    io:format("    -q=<chassisid> or --chassisid=<chassisid> = force a chassis id (see note 3)~n"),
    io:format("    -t, --dtb=<dtb file>                      = specify a new dtb file~n"),
    io:format("~n"),
    io:format("Commands list: ~n"),
    io:format("    -h, --help                                     = show this message help~n"),
    io:format("    -c, --check                                    = check for all device versions~n"),
    io:format("    -u=<device> or --update=<device>               = perform to device update (see note 4)~n"),
    io:format("    -d=<device/alias> or --disable=<device/alias>  = disable device from list (see note 4)~n"),
    io:format("    -e=<device/alias> or --enable=<device/alias>   = enable device from list (see note 4)~n"),
    io:format("    -x=<device> or --examine=<device>  = examine for the current alias (see note 4)~n"),
    io:format(" ~n"),
    io:format(" Notes:~n"),
    io:format("           1) verbose mode levels~n"),
    io:format("              -v=0  no message to be shown~n"),
    io:format("              -v=1  includes 'err', 'emerg', 'alert' and 'crit' messages~n"),
    io:format("              -v=2  includes 'warning' and 'notice' messages~n"),
    io:format("              -v=3  includes 'info' messages~n"),
    io:format("              -v=4  includes 'debug' messages~n"),
    io:format(" ~n"),
    io:format("           2) card id values must be the card where the script is running. ~n"),
    io:format("              -o=lc1~n"),
    io:format("              -o=lc4~n"),
    io:format("              -o=lc5~n"),
    io:format("              -o=mngt~n"),
    io:format("              -o=sc2000~n"),
    io:format(" ~n"),
    io:format("               Note: If no card id is specified then the script will try discovery it.~n"),
    io:format(" ~n"),
    io:format("           3) chassis id values must be the chassis where the script is running~n"),
    io:format("              -q=etsc1~n"),
    io:format("              -q=etsc2~n"),
    io:format("              -q=etsc6~n"),
    io:format(" ~n"),
    io:format("              Note: If no chassis id is specified then the script will try discovery it.~n"),
    io:format(" ~n"),
    io:format("           4) Supported devices for update in this board:~n"),
    io:format(" ~n"),

    show_devices("lc1", "1"),

    io:format(" ~n"),
    io:format(" Examples:~n"),
    io:format("       1) Checking for all devices versions (no verbose):~n"),
    io:format("          # ~s -c~n", [?MODULE_NAME]),
    io:format(" ~n"),
    io:format("       2) Checking for all devices versions (verbose mode):~n"),
    io:format("          # ~s -v=3 -c ~n", [?MODULE_NAME]),
    io:format(" ~n"),
    io:format("       3) Checking for all versions and update them automatically (verbose mode):~n"),
    io:format("          # ~s -v=3 -a -c~n", [?MODULE_NAME]),
    io:format(" ~n"),
    io:format("       4) Updating a specified device (verbose mode):~n"),
    io:format("          # ~s -v=3 -u=lc1_lpc55~n", [?MODULE_NAME]),
    io:format(" ~n"),
    io:format("       5) Disabling a specified device (verbose mode):~n"),
    io:format("          # ~s -v=3 -d=lc1_fpgajic/otu2~n", [?MODULE_NAME]),
    io:format(" ~n"),
    io:format("       6) Enabling a specified device, automaticaly updating and~n"),
    io:format("          rebooting the board after the update (verbose mode):~n"),
    io:format("          # ~s -v=3 -r -e=lc1_fpgajic/otu2~n", [?MODULE_NAME]),
    io:format(" ~n"),
    io:format("       7) Running in background mode avoiding interruption during device update:~n"),
    io:format("          # ~s -g -r -e=lc1_fpgajic/otu2~n", [?MODULE_NAME]),
    io:format(" ~n").

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec show_devices(string, string) -> {result, string}.
show_devices(CurrentBoard, Active) ->
    {Result1, NumBoardsStr} = ini_file(?INI_FILE, "boards", "num_boards"),

    case Result1 of
        ok ->
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            show_devices_next_board(CurrentBoard, Active, 1, NumBoards);

        _ ->
            error
    end.

show_devices_next_board(CurrentBoard, Active, Index, MaxBoards) ->
    if
        (Index =< MaxBoards) ->
            {Result1, Board} = ini_file(?INI_FILE, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

            if
                (Result1 == ok) ->
                    %io:format("~s~n", [Board]),

                    {_, NumDevicesStr} = ini_file(?INI_FILE, Board, "num_devices"),
                    {NumDevices, _} = string:to_integer(NumDevicesStr),
                    show_devices_next_Device(Board, CurrentBoard, Active, 1, NumDevices),
                    show_devices_next_board(CurrentBoard, Active, Index + 1, MaxBoards);

                true ->
                    error
            end;

        true ->
            ok
    end.

show_devices_next_Device(Board, CurrentBoard, Active, Index, MaxDevices) ->
    if
        (Index =< MaxDevices) ->
            {Result1, Device} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, Activecard} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
            {Result3, Enabled} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
            
            if
                (Result1 == ok) and (Result2 == ok) and (Result3 == ok) and
                ((CurrentBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                (Enabled == "1") ->
                    io:format("                ~s_~s~n", [Board, Device]),
                    show_devices_next_Device(Board, CurrentBoard, Active, Index + 1, MaxDevices);

                true ->
                    show_devices_next_Device(Board, CurrentBoard, Active, Index + 1, MaxDevices)
            end;

        true ->
            ok
    end.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec show_boards_tree(string, string) -> ok | error.
show_boards_tree(CurrentBoard, Active) ->
    {Result1, NumBoardsStr} = ini_file(?INI_FILE, "boards", "num_boards"),

    case Result1 of
        ok ->
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            show_boards_tree_next_board(CurrentBoard, Active, 1, NumBoards);

        _ ->
            error
    end.

show_boards_tree_next_board(CurrentBoard, Active, Index, MaxBoards) ->
    if
        (Index =< MaxBoards) ->
            {Result1, Board} = ini_file(?INI_FILE, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

            if
                (Result1 == ok) ->
                    %io:format("~s~n", [Board]),

                    {_, NumDevicesStr} = ini_file(?INI_FILE, Board, "num_devices"),
                    {NumDevices, _} = string:to_integer(NumDevicesStr),
                    show_boards_tree_next_device(Board, CurrentBoard, Active, 1, NumDevices, 0),
                    show_boards_tree_next_board(CurrentBoard, Active, Index + 1, MaxBoards);

                true ->
                    error
            end;

        true ->
            ok
    end.

show_boards_tree_next_device(Board, CurrentBoard, Active, Index, MaxDevices, Flag) ->
    if
        (Index =< MaxDevices) ->
            {Result1, Device} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, Activecard} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
            {Result3, Enabled} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
            {Result4, Alias} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),

            if
                (Result1 == ok) and (Result2 == ok) and (Result3 == ok) and (Result4 == ok) and
                ((CurrentBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                (Enabled == "1") and (Alias == "") and
                (Flag == 0) ->
                    io:format("~n"),
                    io:format("             ~s~n", [Board]),
                    io:format("              └── ~s~n", [Device]),
                    show_boards_tree_next_device(Board, CurrentBoard, Active, Index + 1, MaxDevices, Flag + 1);

                (Result1 == ok) and (Result2 == ok) and (Result3 == ok) and (Result4 == ok) and
                ((CurrentBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                (Enabled == "1") and (Alias == "") and
                (Flag /= 0) ->
                    io:format("              └── ~s~n", [Device]),
                    show_boards_tree_next_device(Board, CurrentBoard, Active, Index + 1, MaxDevices, Flag + 1);

                (Result1 == ok) and (Result2 == ok) and (Result3 == ok) and (Result4 == ok) and
                ((CurrentBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                (Enabled == "1") and (Alias /= "") and
                (Flag == 0) ->
                    io:format("~n"),
                    io:format("             ~s~n", [Board]),
                    io:format("              └── ~s (~s)~n", [Device, Alias]),
                    show_boards_tree_next_device(Board, CurrentBoard, Active, Index + 1, MaxDevices, Flag + 1);

                (Result1 == ok) and (Result2 == ok) and (Result3 == ok) and (Result4 == ok) and
                ((CurrentBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                (Enabled == "1") and (Alias /= "") and
                (Flag /= 0) ->
                    io:format("              └── ~s (~s)~n", [Device, Alias]),
                    show_boards_tree_next_device(Board, CurrentBoard, Active, Index + 1, MaxDevices, Flag + 1);

                true ->
                    show_boards_tree_next_device(Board, CurrentBoard, Active, Index + 1, MaxDevices, Flag)
            end;

        true ->
            ok
    end.


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
-spec check_for_supported_board(string) -> boolean.
check_for_supported_board(Board) ->
    {Result1, NumBoardsStr} = ini_file(?INI_FILE, "boards", "num_boards"),

    case Result1 of
        ok ->
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            check_for_supported_board(Board, 1, NumBoards);

        _ ->
            error
    end.

check_for_supported_board(Board, Index, MaxBoards) ->
    if
        (Index =< MaxBoards) ->
            {Result1, BoardRead} = ini_file(?INI_FILE, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

            if
                (Result1 == ok) and (BoardRead == Board) ->
                    ok;

                true ->
                    check_for_supported_board(Board, Index + 1, MaxBoards)
            end;

        true ->
            error
    end.


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

update_cfg_file_next_board(Source, Target, Index, MaxBoards) ->
    if
        (Index =< MaxBoards) and (MaxBoards > 0) ->
            {Result1, Board} = ini_file(Source, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

            if
                (Result1 == ok) ->
                    {_, NumDevicesStr} = ini_file(Source, Board, "num_devices"),
                    {NumDevices, _} = string:to_integer(NumDevicesStr),
                    update_cfg_file_next_Device(Source, Target, Board, 1, NumDevices),
                    update_cfg_file_next_board(Source, Target, Index + 1, MaxBoards);

                true ->
                    error
            end;

        true ->
            ok
    end.

update_cfg_file_next_Device(Source, Target, Board, Index, MaxDevices) ->
    if
        (Index =< MaxDevices) ->
            {Result1, Device} = ini_file(Source, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, Enabled} = ini_file(Source, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
            {Result3, Alias} = ini_file(Source, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),
            
            if
                (Result1 == ok) and (Result2 == ok) and (Result3 == ok) ->
                    {_, IndexTarget} = get_device_index_from_cfg(Target, Board, Device, "alias", Alias),
                    ini_file(Target, Board, unicode:characters_to_list(["enabled", integer_to_list(IndexTarget)]), Enabled, wr),
                    update_cfg_file_next_Device(Source, Target, Board, Index + 1, MaxDevices);

                true ->
                    error
            end;

        true ->
            ok
    end.


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
-spec get_platform_type() -> ok.
get_platform_type() ->
    ok.


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_dtb_name() -> ok.
get_dtb_name() ->
    ok.


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_board_type() -> ok.
get_board_type() ->
    ok.


    