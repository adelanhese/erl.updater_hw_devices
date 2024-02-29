%
% 1) To compile:compile
%    erlc *.erl
%
% 2) To push
%    git push -u origin main
%
-module(updater_hw_devices).
-include("updater_hw_devices_defines.hrl").

-export([main/1,
         get_version/5,
         update/3,
         check_versions/5,
         power_cycle/2,
         get_slot_id/2,
         check_for_dependencies/1,
         dependencies_list_etsc1/0,
         dependencies_list_etsc2/0,
         dependencies_list_etsc6/0,
         dependencies_list_test/0,
         check_parameters/6,

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


dependencies_list_etsc1() -> [?FPGAIO,
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

dependencies_list_etsc2() -> [?FPGAIO,
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

dependencies_list_etsc6() -> [?FPGAIO,
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
-spec get_version(string, string, string, string,string) -> {result, string}.
get_version(CfgFileName, Platform, Board, Device, LocalPartNumber) -> 
    ModuleName = unicode:characters_to_list([?MODULE_NAME,"_", Platform]),
    FuncName = unicode:characters_to_list([atom_to_list(?FUNCTION_NAME),"_", Platform, "_", Board, "_", Device]),
    call_function(ModuleName, FuncName, [CfgFileName, LocalPartNumber]).

%-----------------------------------------------------------------------------
% Checcks for the parameters:
%     Platform
%     Board
%     Device
% 
%-----------------------------------------------------------------------------
-spec check_parameters(string, string, string, string, string, string) -> {result, string}.
check_parameters(Command, CfgFileName, Platform, BaseBoard, Device_to_update, Active) -> 
    Board = updater_hw_devices_utils:extract_board(Device_to_update),
    Device = updater_hw_devices_utils:extract_device(Device_to_update),
    CheckDevice = updater_hw_devices_cfgfileparse:check_for_supported_devices(CfgFileName, Board, Device, Board, Active, ""),
    CheckdBoard = updater_hw_devices_cfgfileparse:check_for_supported_board(CfgFileName, Board),
    CheckdBaseBoard = updater_hw_devices_cfgfileparse:check_for_supported_board(CfgFileName, BaseBoard),
    CheckPlatform = updater_hw_devices_utils:check_for_supported_platform(Platform),
    check_parameters(Command, {CheckPlatform, Platform}, {CheckdBaseBoard, BaseBoard}, {CheckdBoard, Board}, {CheckDevice, Device}).
check_parameters(_Command, {true,_Platform}, {ok, _BaseBoard}, {ok, _Board}, {ok, _Device}) -> 
    {ok, "Valid"};
check_parameters(_Command,{false, Platform}, {_, _BaseBoard}, {_, _Board}, {_, _Device}) ->
    {error, unicode:characters_to_list(["ERROR: Platform not supported: ", Platform])};
check_parameters(_Command,{_, _Platform}, {error, BaseBoard}, {_, _Board}, {_, _Device}) -> 
    {error, unicode:characters_to_list(["ERROR: Base board not supported: ", BaseBoard])};
check_parameters(Command,{_, _Platform}, {_, _BaseBoard}, {error, Board}, {_, _Device}) when (Command =/= check) and (Command =/= showhelp) ->
    {error, unicode:characters_to_list(["ERROR: Board not supported: ", Board])};
check_parameters(Command,{_, _Platform}, {_, _BaseBoard}, {_, _Board}, {error, Device}) when (Command =/= check) and (Command =/= showhelp) ->
    {error, unicode:characters_to_list(["ERROR: Device not supported: ", Device])};
check_parameters(Command, {true,_Platform}, {ok, _BaseBoard}, {_, _Board}, {_, _Device}) when (Command == check) or (Command == showhelp) ->
     {ok, "Valid"}.
        
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
%  ToDo: to indlude 2ek
%-----------------------------------------------------------------------------
-spec check_versions(string, string, string, string, string) -> {result, string}.
check_versions(CfgFileName, Platform, BaseBoard, Active, LocalPartNumber) ->
    {Result, NumBoardsStr} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, "boards", "num_boards"),
    check_versions({Result, NumBoardsStr}, CfgFileName, Platform, BaseBoard, Active, LocalPartNumber).
check_versions({ok, NumBoardsStr}, CfgFileName, Platform, BaseBoard, Active, LocalPartNumber) ->
    updater_hw_devices_cfgfileparse:show_boards_tree(CfgFileName, BaseBoard, Active, LocalPartNumber),
    io:format("~n"),
    {NumBoards, _} = string:to_integer(NumBoardsStr),
    check_versions_next_board(CfgFileName, Platform, BaseBoard, Active, 1, NumBoards, LocalPartNumber);
check_versions({_, NumBoardsStr}, _IniFile, _Platform, _BaseBoard, _Activ, _LocalPartNumbere) ->
    io:format("Error: ~p~n", [NumBoardsStr]),
    {error,  unicode:characters_to_list(["ERROR: Invalid board index: ", NumBoardsStr])}.

% Next board
check_versions_next_board(_IniFile, _Platform, _BaseBoard, _Active, Index, MaxBoards, _LocalPartNumber)  when (Index > MaxBoards)->
    {ok, "All boards was checked"};
check_versions_next_board(CfgFileName, Platform, BaseBoard, Active, Index, MaxBoards, LocalPartNumber) ->
    {Result1, Board} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, "boards", unicode:characters_to_list(["board", integer_to_list(Index)])),
    {Result2, Chrono2ekStr} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, Board, "chrono_2ek"),
    check_versions_next_board({Result1, Board}, {Result2, Chrono2ekStr}, CfgFileName, Platform, BaseBoard, Active, Index, MaxBoards, LocalPartNumber).

check_versions_next_board({ok, Board}, {ok, Chrono2ekStr}, CfgFileName, Platform, BaseBoard, Active, Index, MaxBoards, LocalPartNumber) ->
    {_, NumDevicesStr} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, Board, "num_devices"),
    {NumDevices, _} = string:to_integer(NumDevicesStr),
    check_versions_next_device(CfgFileName, Platform, Board, BaseBoard, Active, 1, NumDevices, Chrono2ekStr, LocalPartNumber),
    check_versions_next_board(CfgFileName, Platform, BaseBoard, Active, Index + 1, MaxBoards, LocalPartNumber);
check_versions_next_board({_Result1, _Board}, {_Result2, _Chrono2ekStr}, _IniFile, _Platform, _BaseBoard, _Active, _Index, _MaxBoards, _LocalPartNumber) ->
    {error, "No board found"}.

% Next device
check_versions_next_device(_IniFile, _Platform, _Board, _BaseBoard, _Active, Index, MaxDevices, _Chrono2ekStr, _LocalPartNumber) when  (Index > MaxDevices)->
    {ok, "All devices was checked"};
check_versions_next_device(CfgFileName, Platform, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber)->
   {Result1, Device} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["device", integer_to_list(Index)])),
   {Result2, Activecard} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["activecard", integer_to_list(Index)])),
   {Result3, Enabled} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["enabled", integer_to_list(Index)])),
   {Result4, Checkversion} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["checkversion", integer_to_list(Index)])),
   {Result5, VfVrIcs2ekStr} = updater_hw_devices_cfgfileparse:read_cfg_file(CfgFileName, Board, unicode:characters_to_list(["vf_vr_ics_2ek", integer_to_list(Index)])),
   Board2ekMatch = updater_hw_devices_cfgfileparse:board_2ek_match(Chrono2ekStr, VfVrIcs2ekStr, LocalPartNumber),
   check_versions_next_device({Result1, Device}, {Result2, Activecard}, {Result3, Enabled}, {Result4, Checkversion}, {Result5, Board2ekMatch}, CfgFileName, Platform, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber).
check_versions_next_device({ok, Device}, {ok, Activecard}, {ok, "1"}, {ok, "1"}, {ok, match}, CfgFileName, Platform, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) when (BaseBoard == Board) or (((Activecard == Active)) and (Active == "1")) ->
    {_, VersionFromDevice} = get_version(CfgFileName, Platform, Board, Device, LocalPartNumber),
    {_, VersionFromIniFile} = updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, Board, Device, "version", ""),
    {_, ResultCompare} = check_versions_compare(VersionFromDevice, VersionFromIniFile),  
    io:format("[~s_~s] = ~s => ~s ~n", [Board, Device, VersionFromDevice, ResultCompare]),
    check_versions_next_device(CfgFileName, Platform, Board, BaseBoard, Active, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber);
check_versions_next_device({ok, _Device}, {ok, _Activecard}, {ok, _Enabled}, {ok, _Checkversion}, {ok, _Board2ekMatch}, CfgFileName, Platform, Board, BaseBoard, Active, Index, MaxDevices, Chrono2ekStr, LocalPartNumber) ->
    check_versions_next_device(CfgFileName, Platform, Board, BaseBoard, Active, Index + 1, MaxDevices, Chrono2ekStr, LocalPartNumber).

% compare the versions
check_versions_compare(FromDevice, FromIniFile) when (FromDevice =/= "") and (FromIniFile =/= "") and (FromDevice == FromIniFile) ->
    {ok, "updated"};
check_versions_compare(_FromDevice, _FromIniFile)->
    {error, "outdated"}.

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
                   port => "",
                   part_number => "",
                   undefined => ""},

    {ok, OptionsMap1} = updater_hw_devices_cmdlineparse:parse_params(Args, 1, OptionsMap),
    {ok, Platform_type} = updater_hw_devices_utils:get_platform_type(maps:get(platform_type, OptionsMap1)),
    {ok, Board_type} = updater_hw_devices_utils:get_board_type(maps:get(board_type, OptionsMap1)),
    {ok, Active} = updater_hw_devices_utils:get_active(maps:get(active, OptionsMap1)),
    Device_to_update = maps:get(device_to_update, OptionsMap1),
    Hw_image_partition = maps:get(hw_image_partition, OptionsMap1),
    CfgFileName = unicode:characters_to_list([Hw_image_partition, ?IMAGES_PATH, Platform_type, "/", Platform_type, "_devices", ?CFG_TYPE_FILE]),
    {_, LocalPartNumber} = updater_hw_devices_utils:get_part_number(maps:get(part_number, OptionsMap1)),
    Command = maps:get(command, OptionsMap1),
    %io:format("map:   ~p~n", [OptionsMap1]),
    io:format("Current platform:   ~p~n", [Platform_type]),
    io:format("Current base board: ~p~n", [Board_type]),
    io:format("Current partnumber: ~p~n", [LocalPartNumber]),
    {Result, Detail} = check_parameters(Command, CfgFileName, Platform_type, Board_type, Device_to_update, Active),
    main({Result, Detail}, Command, CfgFileName, Platform_type, Board_type, Active, Device_to_update, LocalPartNumber);
main(_Args) ->
    updater_hw_devices_cmdlineparse:show_help("","","").
  
main({ok, _Detail}, Command, CfgFileName, Platform_type, Board_type, Active, Device_to_update, LocalPartNumber) ->
    {Result, Detail} = command_run(Command, CfgFileName, Platform_type, Board_type, Active, Device_to_update, LocalPartNumber),
    io:format("~p: ~p~n", [Result, Detail]);
main({_, Detail}, _Command, _IniFile, _Platform_type, _Board_type, _Active, _Device_to_update, _LocalPartNumber) ->
    io:format("~p~n", [Detail]).

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
-spec command_run(string, string, string, string, string, string, string) -> {result, string}.
command_run(show_help, CfgFileName, _Platform_type, Board_type, Active, _Device_to_update, _LocalPartNumber) ->
    updater_hw_devices_cmdlineparse:show_help(CfgFileName, Board_type, Active),
    {ok, ""};
command_run(check, CfgFileName, Platform_type, Board_type, Active, _Device_to_update, LocalPartNumber) ->
    check_versions(CfgFileName, Platform_type, Board_type, Active, LocalPartNumber);
command_run(update, _IniFile, Platform_type, _Board_type, _Active, Device_to_update, _LocalPartNumber) ->
    Board = updater_hw_devices_utils:extract_board(Device_to_update),
    Device = updater_hw_devices_utils:extract_device(Device_to_update),
    update(Platform_type, Board, Device);
command_run(disable, CfgFileName, _Platform_type, Board_type, Active, Device_to_update, LocalPartNumber) ->
    updater_hw_devices_cfgfileparse:enable_disable_device(CfgFileName, Device_to_update, "0", Board_type, Active, LocalPartNumber);
command_run(enable, CfgFileName, _Platform_type, Board_type, Active, Device_to_update, LocalPartNumber) ->
    updater_hw_devices_cfgfileparse:enable_disable_device(CfgFileName, Device_to_update, "1", Board_type, Active, LocalPartNumber);
command_run(examine, CfgFileName, _Platform_type, _Board_type, _Active, Device_to_update, LocalPartNumber) ->
    Board = updater_hw_devices_utils:extract_board(Device_to_update),
    Device = updater_hw_devices_utils:extract_device(Device_to_update),
    updater_hw_devices_cfgfileparse:read_field_from_cfg(CfgFileName, Board, Device, "alias", LocalPartNumber);
command_run(_Command, _IniFile, _Platform_type, _Board_type, _Active, _Device_to_update, _LocalPartNumber) ->
    {error, "Invalid command"}.

