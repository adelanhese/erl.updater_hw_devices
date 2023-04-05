% usar a função bubble_sort/1,
%  salve o código acima em um arquivo com extensão .erl (por
%  exemplo, my_module.erl)
%  e compile-o usando o comando erlc my_module.erl.
%  Em seguida, abra o shell do Erlang com o comando erl e
%  carregue o módulo usando o comando my_module:module_info()..
%  Você pode então chamar a função bubble_sort/1 com
%  uma lista de 10 elementos numéricos como argumento:
% 
% 1> my_module:bubble_sort([5, 2, 9, 1, 7, 3, 8, 6, 10, 4]).
% [1,2,3,4,5,6,7,8,9,10]
%
%
%   git push -u origin main
%
-module(my_lib).
-export([bubble_sort/1]).
-export([update_var/2]).
-export([procura_string/2]).
-export([procura_string_arquivo/2]).
-export([show_menu/0]).
-export([read_option1/0]).
-export([read_option2/0]).
-export([read_option3/0]).
-export([print_loop/1]).
-export([print_loop1/2]).
-export([print_loop2/2]).
-export([print_loop3/2]).
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
-export([get_device_index_from_cfg/4]).
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




-define(INI_FILE, "arq.ini").
-define(IMAGES_PATH, "/opt/ets/hw/images/").
-define(SPK_PARTITION, "/mnt/update").
-define(KERNEL_CMDLINE, "/proc/cmdline").
-define(KERNEL_BZIMAGE, "/boot/bzImage").
-define(KEXEC_TIMEOUT, "2").
-define(DEVICES_VERSIONS_FILE, "/data/board/hw_devices_versions.csv").


-define(GPIOGET, "/usr/bin/gpioget").
-define(GPIOSET, "/usr/bin/gpioset").
-define(GPIOFIND, "/usr/bin/gpiofind").
-define(FPGAIO, "/usr/bin/fpgaio").
-define(I2CGET, "/usr/sbin/i2cget").
-define(I2CSET, "/usr/sbin/i2cset").
-define(I2CTRANSFER, "/usr/sbin/i2ctransfer").
-define(I2CDETECT, "/usr/sbin/i2cdetect").
-define(FAN, "/usr/bin/fan").
-define(DD, "/bin/dd").
-define(BTOOL, "/usr/bin/btool").
-define(MD5SUM, "/usr/bin/md5sum").
-define(MX25U256, "/dev/mx25u256").
-define(MACHXO2, "/usr/bin/machxo2").
-define(BLHOST, "/usr/bin/blhost").
-define(KEXEC, "/usr/sbin/kexec").
-define(ISSI_FLASH, "/usr/bin/issi_flash").
-define(SPI_CPLD_DRIVER, "/dev/spidev50.2").
-define(DATA_BOARD_PATH, "/data/board/").
-define(FLASHROM, "/usr/sbin/flashrom").
-define(EHALCLI, "/usr/bin/ehalcli").

-define(ETSC1, "etsc1").
-define(ETSC2, "etsc2").
-define(ETSC6, "etsc6").
-define(TEST, "test").

-define(NOHUP, "/usr/bin/nohup").
-define(NOTIFY_SEND, "/usr/bin/notify-send").
-define(NPROC, "/usr/bin/nproc").
-define(NROFF, "/usr/bin/nroff").
-define(NSENTER, "/usr/bin/nsenter").
-define(NSLOOKUP, "/usr/bin/nslookup").
-define(NSS_ADDBUILTIN, "/usr/bin/nss-addbuiltin").
-define(NSS_DBTEST, "/usr/bin/nss-dbtest").
-define(NSS_PP, "/usr/bin/nss-pp").
-define(NSTAT, "/usr/bin/nstat").
-define(NSUPDATE, "/usr/bin/nsupdate").
-define(NTFS_3G, "/usr/bin/ntfs-3g").
-define(NTFS_3G_PROBE, "/usr/bin/ntfs-3g.probe").
-define(NTFSCAT, "/usr/bin/ntfscat").
-define(NTFSCLUSTER, "/usr/bin/ntfscluster").
-define(NTFSCMP, "/usr/bin/ntfscmp").
-define(NTFSDECRYPT, "/usr/bin/ntfsdecrypt").
-define(NTFSFALLOCATE, "/usr/bin/ntfsfallocate").
-define(NTFSFIX, "/usr/bin/ntfsfix").
-define(NTFSINFO, "/usr/bin/ntfsinfo").
-define(NTFSLS, "/usr/bin/ntfsls").
-define(NTFSMOVE, "/usr/bin/ntfsmove").
-define(NTFSRECOVER, "/usr/bin/ntfsrecover").
-define(NTFSSECAUDIT, "/usr/bin/ntfssecaudit").
-define(NTFSTRUNCATE, "/usr/bin/ntfstruncate").
-define(NTFSUSERMAP, "/usr/bin/ntfsusermap").
-define(NTFSWIPE, "/usr/bin/ntfswipe").
-define(NUMFMT, "/usr/bin/numfmt").
-define(NVIDIA_DETECTOR, "/usr/bin/nvidia-detector").
-define(NVLC, "/usr/bin/nvlc").

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

%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
%
%
%
update_var(Var, Valor) ->
    Var = Valor.



%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
% Função que mostra o menu e retorna a opção escolhida pelo usuário
show_menu() ->
    io:format("Escolha uma das opções abaixo:~n"),
    io:format("1. Opção 1~n"),
    io:format("2. Opção 2~n"),
    io:format("3. Opção 3~n"),
    io:format("4. Opção 4~n"),
    io:format("5. Opção 5~n"),
    read_option3().

% Função auxiliar que lê e verifica se a opção escolhida é válida
read_option1() ->
    {ok, Opcao} = io:fread("Sua opção-> ", "~u"),
    case  lists:nth(1, Opcao) of
        1 ->
            io:format("Opção válida.~n");
        _ ->
            io:format("Opção inválida.~n"),
            read_option1()
    end.    

read_option2() ->
    {ok, Opcao} = io:fread("Sua opção-> ", "~u"),

    A = lists:nth(1, Opcao),

    case A of
        _ when (A < 1) or (A > 5) ->
            io:format("Opção inválida. Escolha uma opção válida.~n"),
            read_option2();
        _ ->
            A
    end.

read_option3() ->
        {ok, Opcao} = io:fread("Sua opção-> ", "~u"),
    
        A = lists:nth(1, Opcao),
    
        if
           (A < 1) or (A > 5) ->
                B = A,
                io:format("~p é uma opção inválida. Escolha uma opção válida.~n", [B]),
                read_option3();
            true ->
                {ok, A}
        end.


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
% imprime uma mensagem N vezes
print_loop(N) ->
            print_loop3(1, N).
        
print_loop1(I, N) when I =< N ->
            io:format("Mensagem ~w~n", [I]),
            print_loop1(I+1, N);
        print_loop1(_, _) ->
            ok.

print_loop2(I, N) ->

        case N of
           _ when (I =< N) ->
                   io:format("Mensagem ~w~n", [I]),
                   print_loop2(I+1, N);
           _ ->
                   ok
        end.

print_loop3(I, N) ->
   if
       (I =< N) ->
          io:format("Mensagem ~w~n", [I]),
          print_loop3(I+1, N);
       true ->
          ok
   end.

%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
% Esta função extrai uma substring de String,
% começando no terceiro caractere e com comprimento de cinco 
% caracteres. Você pode chamar esta função passando uma string como 
% argumento, como no exemplo a seguir:
% 
% 
% Isso irá retornar a substring "lo, w". Você pode ajustar os valores de Start e
% Length para extrair a substring desejada.
extract_substring(String) ->
        Substring = string:substr(3, 5, String),
        Substring.



%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
% sta função encontra a primeira ocorrência de Char em
% tring e retorna seu índice. Você pode chamar esta função passando 
%  caractere e a string como argumentos, como no exemplo a seguir:
% 
% 
% y_module:find_character($l,
% Hello, world!").
find_character(Char, String) ->
            Index = string:str(String, Char),
            Index.
        

%_____________________________________________________________________________________________
%
%    Utils for updater_hw_devices
%_____________________________________________________________________________________________
    
% Arguments:
%   arg1: Composite string with: <board>_<device>/<alias>
% Returns:
%   device name
%
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
 

% Arguments:
%   arg1: Composite string with: <board>_<device>/<alias>
% Returns:
%   device name
%
%extract_device() {
%    str="$1"
%    device="${str##*_}"
%
%    if [[ $str == *"/"* ]]; then
%        alias="${device##*/}"
%        device=${device%/${alias}*}
%    fi
%
%    echo $device
%}
extract_device(BoardDevice) ->
    {Status, _, Device, _} = extract_board_device(BoardDevice),
    
    case Status of
        ok ->
            Device;
        error ->
            ""
    end.



% Arguments:
%   arg1: Composite string with: <board>_<device>/<alias>
% Returns:
%   board name
%
%extract_board() {
%    str="$1"
%    device="${str##*_}"
%    board=${str%_${device}*}
%    echo $board
%}
extract_board(BoardDevice) ->
    {Status, Board, _, _} = extract_board_device(BoardDevice),
    
    case Status of
        ok ->
            Board;
        error ->
            ""
    end.


% Arguments:
%   arg1: Composite string with: <board>_<device>/<alias>
% Returns:
%   alias name
%
%extract_alias() {
%    str="$1"
%    alias="${str##*/}"
%    echo $alias
%}
extract_alias(BoardDevice) ->
    {Status, _, _, Alias} = extract_board_device(BoardDevice),


    case Status of
        ok ->
            Alias;
        error ->
            ""
    end.

% Arguments:
%   arg1: Composite string with: <board>_<device>/<alias>
% Returns:
%   alias name
%
concatena_strings(Data) ->
    unicode:characters_to_list(Data).


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
list_replace(List, Index, NewValue) ->
    lists:sublist(List, Index-1) ++ [NewValue] ++ lists:nthtail(Index, List).


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
list_delete(List, Index) ->
    [Head | Tail] = List,

    case Index of
        1 -> Tail;
        _ -> [Head | list_delete(Tail, Index-1)]
    end.

%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
list_insert(List, Position, Element) ->
    lists:sublist(List, Position - 1) ++ [Element] ++ lists:nthtail(Position - 1, List).

%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
remove_char(String, Char) ->
    %lists:filter(fun (C) -> C /= Char end, String).
    lists:flatten([C || C <- String, C /= Char]).


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
binary_to_hex(Binary) ->
    lists:flatten([io_lib:format("~2.16.0B", [Byte]) || <<Byte>> <= Binary]).


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
check_file_exists(File) ->
        case filelib:is_file(File) of
            true ->
                {ok, File};
    
            false ->
                {error, "file not found"}
        end.

%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
-spec ini_file(string, string, string) -> {status, string}.
ini_file(IniFile, Sector, Field) ->
    ini_file(IniFile, Sector, Field, "", rd).


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
-spec get_device_index_from_cfg(string, string, string, string) -> {result, number}.
get_device_index_from_cfg(Board, Device, Field, Value) ->
    {Result, NumDevicesStr} = ini_file(?INI_FILE, Board, "num_devices"),

    case Result of
        ok ->
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            get_device_index_from_cfg_search_for_device(Board, Device, Field, Value, 1, NumDevices);

        _ ->
            {error, -1}
    end.

get_device_index_from_cfg_search_for_device(Board, Device, Field, Value, Index, MaxDevices) ->

    if
        (Index =< MaxDevices) ->
            {Result1, DevicesValueStr} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, FieldValueStr} = ini_file(?INI_FILE, Board, unicode:characters_to_list([Field, integer_to_list(Index)])),
            
            if
                (Result1 == ok) and (Result2 == ok) and (DevicesValueStr == Device) and (FieldValueStr == Value) ->
                    {ok, Index};

                true ->
                    get_device_index_from_cfg_search_for_device(Board, Device, Field, Value, Index + 1, MaxDevices)
            end;

        true ->
            {error, -1}
    end.


%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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



%--------------------------------------------------------------
%
%
%--------------------------------------------------------------
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


%--------------------------------------------------------------
%
%
%   io:format("~p ~p ~p ~n", [DevicesStr, EnabledStr, CheckversionStr]),
%
%--------------------------------------------------------------
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



%    Arguments:
%#   none
%# Returns:
%#   String with the path + file name
%#
%enable_disable_device() {
%local device_to_change=$1
%local new_state=$2
%local board=${NULL}
%local device=${NULL}
%local alias=${NULL}
%local index=0
%local res=1
%local num_devices=0
%local device1=${NULL}
%local alias1=${NULL}
%local dependencies1=${NULL}
%local device_dependent=${NULL}
%
%    board=$(extract_board ${device_to_change})
%    device=$(extract_device ${device_to_change})
%    alias=$(extract_alias ${device_to_change})
%
%    check_for_supported_devices ${board}_${device}
%    res=$?
%
%    if [[ $res != ${SUCCESS} ]]; then
%        exit_code=$(code_error $MODULE_MAIN $LINENO $DEVICE_NOT_SUPPORTED_ERROR)
%        log_msg ${ERR} "${device_to_change}: $(error2str $exit_code)"
%        return $exit_code
%    fi
%
%    num_devices=$(read_cfg_file ${board} num_devices)
%    dependencies1=$(read_field_from_cfg_anyway ${board} ${device} dependencies)
%    device_dependent=$(extract_device ${dependencies1})
%
%    for (( d = 1; d <= $num_devices; d++ )); do
%        device1=$(read_cfg_file ${board} device${d})
%        alias1=$(read_cfg_file ${board} alias${d})
%        dependencies1=$(read_cfg_file ${board} dependencies${d})
%
%        if [[ ${device1} == ${device} || ${device1} == ${device_dependent} ]]; then
%            write_cfg_file ${board} enabled${d} 0
%        fi
%
%        if [[ ${device1} == ${device} || ${device1} == ${device_dependent} ]] && [[ ${alias1} == ${alias} ]]; then
%            write_cfg_file ${board} enabled${d} ${new_state}
%        fi
%    done
%
%    return $SUCCESS
%}
%
enable_disable_device(BoardDeviceAlias, NewState, CurrentBoard, Active) ->
    Board = extract_board(BoardDeviceAlias),
    Device = extract_device (BoardDeviceAlias),
    Alias = extract_alias (BoardDeviceAlias),

    io:format("~p ~p ~p ~n", [Board, Device, Alias]),

    case check_for_supported_devices(Board, Device, CurrentBoard, Active) of
        ok ->
            {Result1, Num_devicesStr} = ini_file(?INI_FILE, Board, "num_devices"),
            {Result2, Dependencies1} = read_field_from_cfg_anyway(Board, Device, "dependencies"),
            Device_dependent = extract_device(Dependencies1),
            {Num_devices, _} = string:to_integer(Num_devicesStr),

            io:format("Entry... ~p ~p ~p ~n", [Num_devicesStr, Dependencies1, Device_dependent]),

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

            io:format("Disabling... ~p ~p ~n", [Device1, Device_dependent]),

            if
                (Result1 == ok) and
                ((Device1 == Device) or (Device1 == Device_dependent)) ->
                    ini_file(?INI_FILE, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), "0", wr);

                true ->
                    disable_device(Board, Device, Device_dependent, Index + 1, MaxDevices)
            end;

        true ->
            error
    end.


enable_device(Board, Device, Device_dependent, Alias, NewState, Index, MaxDevices) ->

    if
        (Index =< MaxDevices) ->
            {Result1, Device1} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, Alias1} = ini_file(?INI_FILE, Board, unicode:characters_to_list(["alias", integer_to_list(Index)])),

            io:format("Enabling... ~p ~p ~p ~n", [Device1, Alias1, Device_dependent]),

            if
                (Result1 == ok) and (Result2 == ok)  and
                ((Device1 == Device) or (Device1 == Device_dependent)) and
                (Alias == Alias1) ->
                    ini_file(?INI_FILE, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)]), NewState, wr);

                true ->
                    enable_device(Board, Device, Device_dependent, Alias, NewState, Index + 1, MaxDevices)
            end;

        true ->
            error
    end.
