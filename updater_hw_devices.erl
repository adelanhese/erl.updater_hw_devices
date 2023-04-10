%
%
%   git push -u origin main
%
-module(updater_hw_devices).
-include("updater_hw_devices_defines.hrl").

-export([main/1]).
-export([show_help/0]).
-export([get_version/3]).
-export([update/3]).
-export([check_versions/3]).
-export([power_cycle/2]).
-export([get_slot_id/2]).
-export([check_for_dependencies/1]).
-export([dependencies_list_myplat1/0]).
-export([dependencies_list_myplat2/0]).
-export([dependencies_list_myplat6/0]).
-export([dependencies_list_test/0]).


-export([call_function/3]).

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


dependencies_list_myplat1() -> [?FPGAIO,
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

dependencies_list_myplat2() -> [?FPGAIO,
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

dependencies_list_myplat6() -> [?FPGAIO,
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



%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec call_function(string, string, list) -> result.
call_function(ModuleName, FuncName, Arguments) ->
    erlang:apply(list_to_atom(ModuleName), list_to_atom(FuncName), Arguments).


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_version(string, string, string) -> {result, string}.
get_version(Platform, Board, Device) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME),"_", Platform, "_", Board, "_", Device]),
    call_function(ModuleName, FuncName, []).


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec update(string, string, string) -> {result, string}.
update(Platform, Board, Device) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME),"_", Platform, "_", Board, "_", Device]),
    call_function(ModuleName, FuncName, []).


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec power_cycle(string, string) -> {result, string}.
power_cycle(Platform, Board) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME), "_", Platform, "_", Board]),
    call_function(ModuleName, FuncName, []).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec get_slot_id(string, string) -> {result, string}.
get_slot_id(Platform, Board) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME),"_", Platform, "_", Board]),
    call_function(ModuleName, FuncName, []).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec check_for_dependencies(string) -> ok | error.
check_for_dependencies(Platform) ->
    FuncName = unicode:characters_to_list(["dependencies_list_", Platform]),
    DependenciesList = call_function(?MODULE_NAME, FuncName, []),
    updater_hw_devices_utils:check_for_files_dependencies(DependenciesList, 1).


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
    io:format("    -x=<device> or --examine=<device>              = examine for the current alias (see note 4)~n"),
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
    io:format("              -q=myplat1~n"),
    io:format("              -q=myplat2~n"),
    io:format("              -q=myplat6~n"),
    io:format(" ~n"),
    io:format("              Note: If no chassis id is specified then the script will try discovery it.~n"),
    io:format(" ~n"),
    io:format("           4) Supported devices for update in this board:~n"),
    io:format(" ~n"),

    updater_hw_devices_utils:show_devices("lc1", "1"),

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
-spec check_versions(string, string, string) -> {result, string}.
check_versions(Platform, CurrentBoard, Active) ->
    {Result1, NumBoardsStr} = updater_hw_devices_utils:ini_file(?INI_FILE, "boards", "num_boards"),

    case Result1 of
        ok ->
            updater_hw_devices_utils:show_boards_tree(CurrentBoard, Active),
            io:format("~n"),
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            check_versions_next_board(Platform, CurrentBoard, Active, 1, NumBoards);

        _ ->
            error
    end.

check_versions_next_board(Platform, CurrentBoard, Active, Index, MaxBoards) ->
    if
        (Index =< MaxBoards) ->
            {Result1, Board} = updater_hw_devices_utils:ini_file(?INI_FILE, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

            if
                (Result1 == ok) ->
                    %io:format("~s~n", [Board]),

                    {_, NumDevicesStr} = updater_hw_devices_utils:ini_file(?INI_FILE, Board, "num_devices"),
                    {NumDevices, _} = string:to_integer(NumDevicesStr),
                    check_versions_next_Device(Platform, Board, CurrentBoard, Active, 1, NumDevices),
                    check_versions_next_board(Platform, CurrentBoard, Active, Index + 1, MaxBoards);

                true ->
                    error
            end;

        true ->
            ok
    end.

check_versions_next_Device(Platform, Board, CurrentBoard, Active, Index, MaxDevices) ->
    if
        (Index =< MaxDevices) ->
            {Result1, Device} = updater_hw_devices_utils:ini_file(?INI_FILE, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
            {Result2, Activecard} = updater_hw_devices_utils:ini_file(?INI_FILE, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
            {Result3, Enabled} = updater_hw_devices_utils:ini_file(?INI_FILE, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
            {Result4, Checkversion} = updater_hw_devices_utils:ini_file(?INI_FILE, Board, unicode:characters_to_list(["checkversion", integer_to_list(Index)])),
            
            if
                (Result1 == ok) and (Result2 == ok) and (Result3 == ok) and (Result4 == ok) and
                ((CurrentBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                (Enabled == "1") and (Checkversion == "1")->
                    {_, Resultx} = get_version(Platform, Board, Device),
                    io:format("~s_~s => ~s ~n", [Board, Device, Resultx]),
                    check_versions_next_Device(Platform, Board, CurrentBoard, Active, Index + 1, MaxDevices);

                true ->
                    check_versions_next_Device(Platform, Board, CurrentBoard, Active, Index + 1, MaxDevices)
            end;

        true ->
            ok
    end.




%-----------------------------------------------------------------------------
% 
%
%-----------------------------------------------------------------------------
main(Args) ->

    OptionsMap = #{"background" => false,
                   "auto_update" => false,
                   "show_filename" => false,
                   "spk_updater" => false,
                   "fpga_reload_after_update" => false,
                   "power_cycle_after_update" => false,
                   "system_reboot_after_update" => false,
                   "log_level" => "",
                   "board_type" => "",
                   "platform_type" => "",
                   "dtb_file" => "",
                   "active" => "0",
                   "input_file" => "",
                   "device_to_update" => "",
                   "enable_disable_val" => "0"},

    CommandMap = #{"command" => ""},

    if 
        length(Args) == 0 ->
            show_help();

        true ->
            {ok, OptionsMap1, CommandMap1} = parse_params(Args, 1, OptionsMap, CommandMap),

            Command = maps:get("command", CommandMap1),
            Platform_type = maps:get("platform_type", OptionsMap1),
            Board_type = maps:get("board_type", OptionsMap1),
            Active = maps:get("active", OptionsMap1),
            Device_to_update = maps:get("device_to_update", OptionsMap1),

            case Command of
                "show_help" ->
                    show_help();

               "check" ->
                    check_versions(Platform_type, Board_type, Active);

                "update" ->
                    Board = updater_hw_devices_utils:extract_board(Device_to_update),
                    Device = updater_hw_devices_utils:extract_device(Device_to_update),
                    update(Platform_type, Board, Device);
                  
                "disable" ->
                    updater_hw_devices_utils:enable_disable_device(Device_to_update, "0", Board_type, Active);

                "enable" ->
                    updater_hw_devices_utils:enable_disable_device(Device_to_update, "1", Board_type, Active);

                "examine" ->
                    Board = updater_hw_devices_utils:extract_board(Device_to_update),
                    Device = updater_hw_devices_utils:extract_device(Device_to_update),
                    {_, Value} = updater_hw_devices_utils:read_field_from_cfg(Board, Device, "alias"),
                    io:format("~s~n", [Value]);

                _ ->
                    io:format("Invalid command~n"),
                error
            end
    end.


%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
parse_params(Args, Index, OptionsMap, CommandMap) ->

    if
        Index =< length(Args) ->
            {Parameter, SubParameter} = updater_hw_devices_utils:split_paramenter(lists:nth(Index, Args), "="),

            if
                % options parameters
                (Parameter == "-g") or (Parameter == "--background") ->
                    parse_params(Args, Index + 1, maps:put("background", true, OptionsMap), CommandMap);

                (Parameter == "-v") or (Parameter == "--verbose") ->
                    parse_params(Args, Index + 1, maps:put("log_level", SubParameter, OptionsMap), CommandMap);
                
                (Parameter == "-o") or (Parameter == "--cardid") ->
                    parse_params(Args, Index + 1, maps:put("board_type", SubParameter, OptionsMap), CommandMap);
                
                (Parameter == "-q") or (Parameter == "--chassisid") ->
                    parse_params(Args, Index + 1, maps:put("platform_type", SubParameter, OptionsMap), CommandMap);
                
                (Parameter == "-t") or (Parameter == "--dtb") ->
                    parse_params(Args, Index + 1, maps:put("dtb_file", SubParameter, OptionsMap), CommandMap);
                
                (Parameter == "-a") or (Parameter == "--auto") ->
                    parse_params(Args, Index + 1, maps:put("auto_update", true, OptionsMap), CommandMap);
                
                (Parameter == "-b") or (Parameter == "--backplane") ->
                    parse_params(Args, Index + 1, maps:put("active", "1", OptionsMap), CommandMap);
                
                (Parameter == "-f") or (Parameter == "--file") ->
                    parse_params(Args, Index + 1, maps:put("show_filename", true, OptionsMap), CommandMap);
                
                (Parameter == "-g") or (Parameter == "--background") ->
                    parse_params(Args, Index + 1, maps:put("background", true, OptionsMap), CommandMap);
                
                (Parameter == "-s") or (Parameter == "--spk") ->
                    parse_params(Args, Index + 1, maps:put("spk_updater", true, OptionsMap), CommandMap);
                
                (Parameter == "-i") or (Parameter == "--input") ->
                    parse_params(Args, Index + 1, maps:put("input_file", SubParameter, OptionsMap), CommandMap);
                
                (Parameter == "-r") or (Parameter == "--fpga-reload") ->
                    parse_params(Args, Index + 1, maps:put("fpga_reload_after_update", true, OptionsMap), CommandMap);
                
                (Parameter == "-p") or (Parameter == "--power-cycle") ->
                    parse_params(Args, Index + 1, maps:put("power_cycle_after_update", true, OptionsMap), CommandMap);
                
                (Parameter == "-l") or (Parameter == "--system-reboot") ->
                    parse_params(Args, Index + 1, maps:put("system_reboot_after_update", true, OptionsMap), CommandMap);



                % commands paramenters
                (Parameter == "-h") or (Parameter == "--help") ->
                    parse_params(Args, Index + 1, OptionsMap, maps:put("command", "show_help", CommandMap));
                
                (Parameter == "-c") or (Parameter == "--check") ->
                    parse_params(Args, Index + 1, OptionsMap, maps:put("command", "check", CommandMap));
                
                (Parameter == "-u") or (Parameter == "--update") ->
                    OptionsMapTemp = maps:put("device_to_update", SubParameter, OptionsMap),
                    parse_params(Args, Index + 1, OptionsMapTemp, maps:put("command", "update", CommandMap));
                
                (Parameter == "-d") or (Parameter == "--disable") ->
                    OptionsMapTemp = maps:put("device_to_update", SubParameter, OptionsMap),
                    OptionsMapTemp2 =maps:put("enable_disable_val", "0", OptionsMapTemp),
                    parse_params(Args, Index + 1, OptionsMapTemp2, maps:put("command", "enable_disable", CommandMap));
                
                (Parameter == "-e") or (Parameter == "--enable") ->
                    OptionsMapTemp = maps:put("device_to_update", SubParameter, OptionsMap),
                    OptionsMapTemp2 = maps:put("enable_disable_val", "1", OptionsMapTemp),
                    parse_params(Args, Index + 1, OptionsMapTemp2, maps:put("command", "enable_disable", CommandMap));
                
                (Parameter == "-x") or (Parameter == "--examine") ->
                    OptionsMapTemp = maps:put("device_to_update", SubParameter, OptionsMap),

                    parse_params(Args, Index + 1, OptionsMapTemp, maps:put("command", "examine", CommandMap));
                                   
                true ->
                    io:format("Unknown parameter: ~s~n", [Parameter]),
                    {ok, OptionsMap, CommandMap}
            end;
        
        true ->
            {ok, OptionsMap, CommandMap}
    end.


