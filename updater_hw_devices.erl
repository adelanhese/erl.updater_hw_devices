%
%
%   git push -u origin main
%
-module(updater_hw_devices).
-include("updater_hw_devices_defines.hrl").

-export([main/1,
         get_version/4,
         update/3,
         check_versions/4,
         power_cycle/2,
         get_slot_id/2,
         check_for_dependencies/1,
         dependencies_list_plat1/0,
         dependencies_list_plat2/0,
         dependencies_list_plat6/0,
         dependencies_list_test/0,

         call_function/3]).

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


dependencies_list_plat1() -> [?FPGAIO,
                               ?I2CGET,
                               ?I2CSET,
                               ?I2CTRANSFER,
                               ?I2CDETECT,
                               ?FANSCRIPT,
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

dependencies_list_plat2() -> [?FPGAIO,
                               ?I2CGET,
                               ?I2CSET,
                               ?I2CTRANSFER,
                               ?I2CDETECT,
                               ?FANSCRIPT,
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

dependencies_list_plat6() -> [?FPGAIO,
                               ?I2CGET,
                               ?I2CSET,
                               ?I2CTRANSFER,
                               ?I2CDETECT,
                               ?FANSCRIPT,
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
% Call the function following the rule:
%     updater_hw_devices_<platform>:get_version_<platform>_<board>_<device>
% 
%-----------------------------------------------------------------------------
-spec get_version(string, string, string, string) -> {result, string}.
get_version(IniFile, Platform, Board, Device) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME),"_", Platform, "_", Board, "_", Device]),
    call_function(ModuleName, FuncName, [IniFile]).


%-----------------------------------------------------------------------------
% Call the function following the rule:
%     updater_hw_devices_<platform>:update_<platform>_<board>_<device>
% 
%-----------------------------------------------------------------------------
-spec update(string, string, string) -> {result, string}.
update(Platform, Board, Device) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME),"_", Platform, "_", Board, "_", Device]),
    call_function(ModuleName, FuncName, []).


%-----------------------------------------------------------------------------
% Call the function following the rule:
%     updater_hw_devices_<platform>:power_cycle_<platform>_<board>
% 
%-----------------------------------------------------------------------------
-spec power_cycle(string, string) -> {result, string}.
power_cycle(Platform, Board) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME), "_", Platform, "_", Board]),
    call_function(ModuleName, FuncName, []).

%-----------------------------------------------------------------------------
% Call the function following the rule:
%     updater_hw_devices_<platform>:get_slot_id_<platform>_<board>
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
-spec check_versions(string, string, string, string) -> {result, string}.
check_versions(IniFile, Platform, BaseBoard, Active) ->
    {Result1, NumBoardsStr} = updater_hw_devices_utils:ini_file(IniFile, "boards", "num_boards"),

    case Result1 of
        ok ->
            updater_hw_devices_utils:show_boards_tree(IniFile, BaseBoard, Active),
            io:format("~n"),
            {NumBoards, _} = string:to_integer(NumBoardsStr),
            check_versions_next_board(IniFile, Platform, BaseBoard, Active, 1, NumBoards);

        _ ->
            io:format("Error: ~p~n", [NumBoardsStr]),
            error
    end.

check_versions_next_board(IniFile, Platform, BaseBoard, Active, Index, MaxBoards) when (Index =< MaxBoards) ->
    {Result1, Board} = updater_hw_devices_utils:ini_file(IniFile, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),

    case Result1 of
        ok ->
            {_, NumDevicesStr} = updater_hw_devices_utils:ini_file(IniFile, Board, "num_devices"),
            {NumDevices, _} = string:to_integer(NumDevicesStr),
            check_versions_next_Device(IniFile, Platform, Board, BaseBoard, Active, 1, NumDevices),
            check_versions_next_board(IniFile, Platform, BaseBoard, Active, Index + 1, MaxBoards);

        _ ->
            error
    end;

check_versions_next_board(_IniFile, _Platform, _BaseBoard, _Active, _Index, _MaxBoards) ->
    ok.

check_versions_next_Device(IniFile, Platform, Board, BaseBoard, Active, Index, MaxDevices) when  (Index =< MaxDevices)->
   {Result1, Device} = updater_hw_devices_utils:ini_file(IniFile, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
   {Result2, Activecard} = updater_hw_devices_utils:ini_file(IniFile, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
   {Result3, Enabled} = updater_hw_devices_utils:ini_file(IniFile, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
   {Result4, Checkversion} = updater_hw_devices_utils:ini_file(IniFile, Board, unicode:characters_to_list(["checkversion", integer_to_list(Index)])),
   
   case {Result1, Result2, Result3, Result4} of
        {ok, ok, ok, ok} when ((BaseBoard == Board) or ((Activecard == Active) and (Active == "1"))) and
                               (Enabled == "1") and (Checkversion == "1")->
                                    {_, Version} = get_version(IniFile, Platform, Board, Device),
                                    io:format("~s_~s => ~s ~n", [Board, Device, Version]),
                                    check_versions_next_Device(IniFile, Platform, Board, BaseBoard, Active, Index + 1, MaxDevices);

       _ ->
           check_versions_next_Device(IniFile, Platform, Board, BaseBoard, Active, Index + 1, MaxDevices)
   end;

check_versions_next_Device(_IniFile, _Platform, _Board, _BaseBoard, _Active, _Index, _MaxDevices) ->
    ok.

%-----------------------------------------------------------------------------
% 
%
%-----------------------------------------------------------------------------
main(Args) when (length(Args) > 0) ->

    OptionsMap = #{background => false,
                   auto_update => false,
                   show_filename => false,
                   spk_updater => false,
                   fpga_reload_after_update => false,
                   power_cycle_after_update => false,
                   system_reboot_after_update => false,
                   log_level => "",
                   board_type => "",
                   platform_type => "",
                   dtb_file => "",
                   active => "0",
                   input_file => "",
                   device_to_update => "",
                   hw_image_partition => "",
                   ini_file => "",
                   command => "",
                   undefined => ""},

    {ok, OptionsMap1} = updater_hw_devices_cmdlineparse:parse_params(Args, 1, OptionsMap),

    {ok, Platform_type} = updater_hw_devices_utils:get_platform_type(maps:get(platform_type, OptionsMap1)),
    {ok, Board_type} = updater_hw_devices_utils:get_board_type(maps:get(board_type, OptionsMap1)),
    {ok, Active} = updater_hw_devices_utils:get_active(maps:get(active, OptionsMap1)),

    Device_to_update = maps:get(device_to_update, OptionsMap1),
    Hw_image_partition = maps:get(hw_image_partition, OptionsMap1),
    IniFile = unicode:characters_to_list([Hw_image_partition, ?IMAGES_PATH, Platform_type, "/", Platform_type, "_devices.cfg"]),
    
    Command = maps:get(command, OptionsMap1),

    %io:format("map: ~p~n", [OptionsMap1]),

    case Command of
        show_help ->
            updater_hw_devices_cmdlineparse:show_help(IniFile, Board_type, Active);

        check ->
            check_versions(IniFile, Platform_type, Board_type, Active);

        update ->
            Board = updater_hw_devices_utils:extract_board(Device_to_update),
            Device = updater_hw_devices_utils:extract_device(Device_to_update),
            update(Platform_type, Board, Device);
          
        disable ->
            updater_hw_devices_utils:enable_disable_device(IniFile, Device_to_update, "0", Board_type, Active);

        enable ->
            updater_hw_devices_utils:enable_disable_device(IniFile, Device_to_update, "1", Board_type, Active);

        examine ->
            Board = updater_hw_devices_utils:extract_board(Device_to_update),
            Device = updater_hw_devices_utils:extract_device(Device_to_update),
            {_, Value} = updater_hw_devices_utils:read_field_from_cfg(Board, Device, "alias"),
            io:format("~s~n", [Value]);

        _ ->
            io:format("Invalid command~n"),
        error
    end;

main(_Args) ->
    updater_hw_devices_cmdlineparse:show_help("","","").

%main_command(show_help, OptionsMap) ->
%    show_help(IniFile, Board_type, Active);
%main_command(check,OptionsMap1) ->
%    check_versions(IniFile, Platform_type, Board_type, Active);
%main_command(update, OptionsMap1) ->
%    Board = updater_hw_devices_utils:extract_board(Device_to_update),
%    Device = updater_hw_devices_utils:extract_device(Device_to_update),
%    update(Platform_type, Board, Device);
%main_command(disable, OptionsMap1) ->
%    updater_hw_devices_utils:enable_disable_device(IniFile, Device_to_update, "0", Board_type, Active);
%main_command(enable, OptionsMap1) ->
%    updater_hw_devices_utils:enable_disable_device(IniFile, Device_to_update, "1", Board_type, Active);
%main_command(examine, OptionsMap1) ->
%    Board = updater_hw_devices_utils:extract_board(Device_to_update),
%    Device = updater_hw_devices_utils:extract_device(Device_to_update),
%    {_, Value} = updater_hw_devices_utils:read_field_from_cfg(Board, Device, "alias"),
%    io:format("~s~n", [Value]);
%main_command(_, _) ->
%    io:format("Invalid command~n"),
%    error.








