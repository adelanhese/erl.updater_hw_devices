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
-export([read_ini_file/1]).
-export([read_ini/3]).
%-export([get_ini_value/3]).
-export([extract_substring/1]).
-export([find_character/2]).
-export([extract_board_device/1]).
-export([extract_device/1]).
-export([extract_board/1]).
-export([extract_alias/1]).
-export([concatena_strings/1]).
%-export([update_ini_field/4]).
-export([list_replace/3]).
-export([list_insert/3]).
-export([ini_file/3]).
-export([ini_file/5]).
-export([list_delete/2]).
-export([remove_char/2]).
-export([read_field_from_cfg/3]).

-define(INI_FILE, "arq.ini").



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
%Esta função lê o conteúdo do arquivo especificado em FilePath usando 
%file:read_file/1, que retorna um valor de tupla 
%{ok, Binary} contendo os dados do arquivo.
%
%
%Os dados do arquivo são convertidos em uma lista de strings usando 
%string:tokens/2 com o caractere de quebra de linha como delimitador. Essa lista é então passada para a função
%
%parse_lines/2, que analisa cada linha e adiciona as tuplas correspondentes à lista de tuplas de acumulador.
%
%
%A função 
%parse_lines/2 usa 
%string:tokens/2 novamente para dividir cada linha em dois campos (chave e valor) separados pelo caractere
%
%=. Se a linha não contiver um sinal de igualdade, é ignorada e a análise continua na próxima linha. Se a linha contiver apenas uma chave, sem valor correspondente, essa chave é ignorada e a análise continua na próxima linha. Se a linha não puder ser
% dividida em dois campos, ela também é ignorada e a análise continua na próxima linha.
%
%
%Ao final, a lista de tuplas contendo os campos e valores é invertida usando 
%lists:reverse/1 para garantir que a ordem dos campos no arquivo .ini seja mantida na lista resultante.
%
read_ini_file(FilePath) ->
    {ok, FileData} = file:read_file(FilePath),
    Lines = string:tokens(binary_to_list(FileData), "\n"),
    parse_lines(Lines, []).

parse_lines([], Acc) ->
    lists:reverse(Acc);
parse_lines([Line | Rest], Acc) ->
    case string:tokens(Line, " =") of
        [Field, Value] ->
            parse_lines(Rest, [{Field, Value} | Acc]);
        [_Field] ->
            parse_lines(Rest, Acc);
        _ ->
            parse_lines(Rest, Acc)
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
            error
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
            error
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
            error
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
read_ini(IniFileName, Sector, Field) ->
    {ok, File} = file:open(IniFileName, [read]),

    case search_for_sector(File, Sector) of
        true ->
            FieldValue = search_for_field(File, Field),
            file:close(File),
            {ok, FieldValue};
        false ->
            file:close(File),
            {error, "Sector not found"}
    end.

% função auxiliar que faz a busca pela string no arquivo
search_for_sector(File, Sector) ->
    case io:get_line(File, "") of
        eof ->
            false;
        Line ->
            case string:str(Line, unicode:characters_to_list(["[", Sector, "]"])) of
                0 ->
                    search_for_sector(File, Sector);
                _ ->
                    true
            end
    end.

search_for_field(File, Field) ->

    Line = io:get_line(File, ""),

    EndOfSector = string:str(Line, "["),

    if
        (EndOfSector > 0) ->
            false;
        true ->
            search_for_field(File, Line, Field)

    end.

search_for_field(File, Line, Field) ->
    case Line of
        eof ->
            false;
        Line ->
            case string:str(Line, unicode:characters_to_list([Field, " = "])) of
                0 ->
                    search_for_field(File, Field);
                _ ->
                    extract_field(Line)
            end
    end.

extract_field(Field) ->
    Index1 = string:str(Field, " = "),

    if
        (Index1 > 0 ) ->
            FieldSize = string:length(Field)-Index1,
            FieldValue = string:slice(Field, Index1+2, abs(FieldSize-3)),
            FieldValue;
        true ->
           error
    end.


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
    {Result, NumDevicesStr} = read_ini(?INI_FILE, Board, "num_devices"),

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
    {Result1, DevicesStr} = read_ini(?INI_FILE, Board, DeviceFieldStr),
    {Result2, EnabledStr} = read_ini(?INI_FILE, Board, EnabledFieldStr),

    if
        (Index =< MaxDevices) ->
            if
                (Result1 == ok) and (Result2 == ok) and (DevicesStr == Device) and (EnabledStr == "1") ->
                    read_ini(?INI_FILE, Board, FieldStr);

                true ->
                    read_field_from_cfg_search_for_device(Board, Device, Field, Index + 1, MaxDevices)
            end;

        true ->
            {error, "field not found"}
    end.




%   read_field_from_cfg_search
%    
%    # Arguments:
%    #   none
%    # Returns:
%    #   0 on success
%    #   1 on fail
%    read_field_from_cfg_anyway() {
%    local board="$1"
%    local device="$2"
%    local field="$3"
%    local num_devices=0
%    local device_cfg=${NULL}
%    local value=${NULL}
%    local enabled=0
%    
%        num_devices=$(read_cfg_file ${board} num_devices)
%    
%        # scan for all devices inside of the board
%        for (( d = 1; d <= $num_devices; d++ )); do
%            device_cfg=$(read_cfg_file ${board} device${d})
%            enabled=$(read_cfg_file ${board} enabled${d})
%    
%             if [[ ${device_cfg} == ${device} ]]; then
%                value=$(read_cfg_file ${board} ${field}${d})
%                break
%             fi
%        done
%    
%        echo $value
%    }
%    
%    # Arguments:
%    #   none
%    # Returns:
%    #   0 on success
%    #   1 on fail
%    get_device_index_from_cfg() {
%    local board="$1"
%    local device="$2"
%    local field="$3"
%    local value="$4"
%    local num_devices=0
%    local device_cfg=${NULL}
%    local value_cfg=${NULL}
%    local index=${NULL}
%    
%        num_devices=$(read_cfg_file ${board} num_devices)
%    
%        # scan for all devices inside of the board
%        for (( d = 1; d <= $num_devices; d++ )); do
%            device_cfg=$(read_cfg_file ${board} device${d})
%            value_cfg=$(read_cfg_file ${board} ${field}${d})
%    
%            if [[ ${device_cfg} == ${device} ]] && [[ ${value_cfg} == ${value} ]]; then
%               index=$d
%               break
%            fi
%        done
%    
%        echo $index
%    }
%    
%    # Arguments:
%    #   none
%    # Returns:
%    #   0 on success
%    #   1 on fail
%    get_file_image_name() {
%    local board="$1"
%    local device="$2"
%    local file=${NULL}
%    local md5sum=${NULL}
%    local md5file=${NULL}
%    
%        if [[ ${input_file} != ${NULL} ]]; then
%            if [[ -e ${input_file} ]]; then
%                file=${input_file}
%                res=$SUCCESS
%            else
%                file=${NULL}
%                res=$FILE_NOT_FOUND_ERROR
%            fi
%    
%            echo $file
%            return $res
%        fi
%    
%        file=$(read_field_from_cfg ${board} ${device} file)
%    
%        if [[ ${file} == ${NULL} ]]; then
%            echo ${NULL}
%            return $INVALID_FILE_NAME_ERROR
%        fi
%    
%        md5file=$(read_field_from_cfg ${board} ${device} md5)
%    
%        if [[ ${md5file} == ${NULL} ]]; then
%            echo ${NULL}
%            return $INVALID_MD5_ERROR
%        fi
%    
%        # check for file integrity
%        if [[ -e ${hw_image_partition}${file} ]]; then
%            md5sum=$(${MD5SUM} ${hw_image_partition}${file} | awk '{print $1}')
%    
%            if [[ ${md5file} != ${md5sum} ]]; then
%                file=${NULL}
%                res=$MD5_CHECK_ERROR
%            else
%                res=$SUCCESS
%            fi
%        else
%            file=${NULL}
%            res=$FILE_NOT_FOUND_ERROR
%        fi
%    
%        echo ${hw_image_partition}$file
%        return $res
%    }
%    