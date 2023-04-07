%
%
%   git push -u origin main
%
-module(updater_hw_devices).
-include("updater_hw_devices_defines.hrl").

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




%check_versions() {
%local outdated_count=0
%local auto_updated_count=0
%local res=0
%local num_boards=0
%local board=${NULL}
%local num_devices=0
%local device=${NULL}
%local file_version=${NULL}
%local device_version=${NULL}
%local file_name=${NULL}
%local run_at_active=0
%local color=${NULL}
%local checkversion=${NULL}
%local enabled=${NULL}
%local base_board=0
%local backplane_board=0
%local state="not checked"
%local restart_type=${NULL}
%
%    log_msg ${INFO} "Checking devices for ${platform_type}"
%
%    num_boards=$(read_cfg_file boards num_boards)
%
%    if [[ 0 == ${spk_updater} ]]; then
%        echo "Platform;Board;Device;Installed;Expected;State;Restart-Type;Estimated-Time" > ${hw_image_partition}${DEVICES_VERSIONS_FILE}
%    fi
%
%    if [[ ${num_boards} == ${NULL} || ${num_boards} == 0 ]]; then
%        log_msg ${INFO} "No board found. Nothing to do."
%        return $SUCCESS
%    fi
%
%    # scan for all boards
%    for (( b = 1; b <= $num_boards; b++ )); do
%        board=$(read_cfg_file boards board${b})
%        num_devices=$(read_cfg_file ${board} num_devices)
%
%        # scan for all devices inside of the board
%        for (( d = 1; d <= $num_devices; d++ )); do
%            run_at_active=$(read_cfg_file ${board} activecard${d})
%            checkversion=$(read_cfg_file ${board} checkversion${d})
%            enabled=$(read_cfg_file ${board} enabled${d})
%            base_board=0
%            backplane_board=0
%            state="not checked"
%
%            if [ ${board} == ${board_type} ]; then
%                base_board=1
%            fi
%
%            if [[ 1 == ${run_at_active}  &&  1 == ${active} ]]; then
%                backplane_board=1
%            fi
%
%            if [[ 1 == ${base_board} || 1 == ${backplane_board} ]] &&  [[ 1 == ${checkversion} ]] && [[ 1 == ${enabled} ]]; then
%                device=$(read_cfg_file ${board} device${d})
%                file_version=$(read_cfg_file ${board} version${d})
%                restart_type=$(read_cfg_file ${board} restart_type${d})
%                estimated_time=$(read_cfg_file ${board} estimated_time${d})
%
%                device_version=$(get_version_${platform_type}_${board}_${device} ${board} ${device})
%                res=$?
%
%                if [[ $res != ${SUCCESS} ]]; then
%                    device_version=$UNKNOWN
%                fi
%
%                if [[ $show_filename == ${TRUE} ]]; then
%                   file_name="    => File = "$(read_cfg_file ${board} file${d})""
%                fi
%
%                color=${RED}
%                if [[ ${file_version} == ${NULL} || ${device_version} == ${NULL} ]]; then
%                    state="not checked"
%                    color=${BYELLOW}
%                fi
%
%                if [[ ${file_version} == ${device_version} && ${file_version} != ${NULL} && ${device_version} != ${NULL} ]]; then
%                   state="updated"
%                   log_msg ${INFO} "${BGREEN}[${board}_${device}] = ${device_version} => ${state} ${RESET}${file_name}"
%                else
%                    state="outdated"
%                    log_msg ${INFO} "${color}[${board}_${device}] Installed = ${device_version} / Expected = ${file_version} => ${state} ${RESET}${file_name}"
%                    outdated_count=$(( outdated_count + 1 ))
%
%                    if [[ $auto_update == ${TRUE} && ${file_version} != ${NULL} && ${device_version} != ${NULL} ]]; then
%                       log_msg ${INFO} "Auto updating..."
%                       update_${platform_type}_${board}_${device} ${board} ${device}
%                       res=$?
%
%                        if [[ $res == ${SUCCESS} ]]; then
%                            outdated_count=$(( outdated_count - 1 ))
%                            auto_updated_count=$(( auto_updated_count + 1))
%                        fi
%                    fi
%                fi
%
%                if [[ 0 == ${spk_updater} ]]; then
%                    echo ${platform_type}";"${board}";"${device}";"${device_version}";"${file_version}";"${state}";"${restart_type}";"${estimated_time} >> ${hw_image_partition}${DEVICES_VERSIONS_FILE}
%                fi
%            fi
%        done
%    done
%
%    if [[ ${outdated_count} != 0 ]]; then
%       log_msg ${INFO} "The ${MODULE_NAME} found ${outdated_count} outdated device(s) or not checked on this board."
%       res=$OUTDATED_ERROR
%    else
%       log_msg ${INFO} "All devices on this board are updated."
%       res=$SUCCESS
%    fi
%
%    if [ ${auto_update} == 1 ] && [ ${auto_updated_count} != 0 ]; then
%       log_msg ${INFO} "   ${auto_updated_count} device(s) was automatically updated."
%       log_msg ${INFO} "   Run ${MODULE_NAME} again to confirm that"
%       res=$SUCCESS
%    fi
%
%    return $res
%}
%

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

