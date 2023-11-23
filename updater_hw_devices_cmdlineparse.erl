-module(updater_hw_devices_cmdlineparse).
-include("updater_hw_devices_defines.hrl").

-export([show_help/3,
         valid_options/0,
         parse_params/3]).

-export_type([options/0]).

-record(options, {
    type                :: atom() | undefined,
    short               :: string() | undefined,
    long                :: string() | undefined,
    map_field           :: atom() | undefined,
    map_value           :: any() | undefined,
    extra_map_field     :: atom() | undefined,
    extra_map_value     :: any() | undefined
    }).

-type options() :: #options{}.

%Value = VarName#record_name.field_name
valid_options() ->
    [#options{type=opt, short="-g", long="--background"   ,map_field=background,                 map_value=true},
     #options{type=opt, short="-v", long="--verbose"      ,map_field=log_level,                  map_value=sub_parameter},
     #options{type=opt, short="-o", long="--cardid"       ,map_field=board_type,                 map_value=sub_parameter},
     #options{type=opt, short="-q", long="--chassisid"    ,map_field=platform_type,              map_value=sub_parameter},
     #options{type=opt, short="-t", long="--dtb"          ,map_field=dtb_file,                   map_value=sub_parameter},
     #options{type=opt, short="-a", long="--auto"         ,map_field=auto_update,                map_value=true},
     #options{type=opt, short="-b", long="--backplane"    ,map_field=active,                     map_value="1"},
     #options{type=opt, short="-f", long="--file"         ,map_field=show_filename,              map_value=true},
     #options{type=opt, short="-s", long="--spk"          ,map_field=spk_updater,                map_value=true,      extra_map_field=hw_image_partition, extra_map_value=?SPK_PARTITION},
     #options{type=opt, short="-i", long="--input"        ,map_field=input_file,                 map_value=sub_parameter},
     #options{type=opt, short="-r", long="--fpga-reload"  ,map_field=fpga_reload_after_update,   map_value=true},
     #options{type=opt, short="-p", long="--power-cycle"  ,map_field=power_cycle_after_update,   map_value=true},
     #options{type=opt, short="-l", long="--system-reboot",map_field=system_reboot_after_update, map_value=true},
     #options{type=cmd, short="-h", long="--help"         ,map_field=command,                    map_value=show_help},
     #options{type=cmd, short="-c", long="--check"        ,map_field=command,                    map_value=check},
     #options{type=cmd, short="-u", long="--update"       ,map_field=command,                    map_value=update,    extra_map_field=device_to_update, extra_map_value=sub_parameter},
     #options{type=cmd, short="-d", long="--disable"      ,map_field=command,                    map_value=disable,   extra_map_field=device_to_update, extra_map_value=sub_parameter},
     #options{type=cmd, short="-e", long="--enable"       ,map_field=command,                    map_value=enable,    extra_map_field=device_to_update, extra_map_value=sub_parameter},
     #options{type=cmd, short="-x", long="--examine"      ,map_field=command,                    map_value=examine,   extra_map_field=device_to_update, extra_map_value=sub_parameter}].

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
show_help(IniFile, Board_type, Active) ->
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
    io:format("              -q=etsc1~n"),
    io:format("              -q=etsc2~n"),
    io:format("              -q=etsc6~n"),
    io:format(" ~n"),
    io:format("              Note: If no chassis id is specified then the script will try discovery it.~n"),
    io:format(" ~n"),
    io:format("           4) Supported devices for update in this board:~n"),
    io:format(" ~n"),

    updater_hw_devices_cfgfileparse:show_devices(IniFile, Board_type, Active),

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
map_value(sub_parameter, SubParameter) ->
    SubParameter;
map_value(MapValue, _SubParameter) ->
    MapValue.

%-----------------------------------------------------------------------------
%
% 
%-----------------------------------------------------------------------------
parse_params(Args, Index, OptionsMap) when (Index > length(Args)) ->
    {ok, OptionsMap};
parse_params(Args, Index, OptionsMap) ->
    {Parameter, SubParameter} = updater_hw_devices_utils:split_paramenter(lists:nth(Index, Args), "="),
    OptionsList = valid_options(),
    Long = lists:keyfind(Parameter, #options.long, OptionsList),
    Short = lists:keyfind(Parameter, #options.short, OptionsList),
    parse_params_next(Args, SubParameter, Index, OptionsMap, Long, Short).


parse_params_next(Args, SubParameter, Index, OptionsMap, Long, false) when (Long /= false) ->
    OptionsExtraMap = maps:put(Long#options.extra_map_field, map_value(Long#options.extra_map_value, SubParameter), OptionsMap),
    parse_params(Args, Index + 1, maps:put(Long#options.map_field, map_value(Long#options.map_value, SubParameter),OptionsExtraMap));
parse_params_next(Args, SubParameter, Index, OptionsMap, false, Short) when (Short /= false) ->
    OptionsExtraMap = maps:put(Short#options.extra_map_field, map_value(Short#options.extra_map_value, SubParameter), OptionsMap),
    parse_params(Args, Index + 1, maps:put(Short#options.map_field, map_value(Short#options.map_value, SubParameter), OptionsExtraMap));
parse_params_next(Args, SubParameter, Index, OptionsMap, Long, Short) when (Long /= false), (Short /= false) ->
    OptionsExtraMap = maps:put(Short#options.extra_map_field, map_value(Short#options.extra_map_value, SubParameter), OptionsMap),
    parse_params(Args, Index + 1, maps:put(Short#options.map_field, map_value(Short#options.map_value, SubParameter), OptionsExtraMap));
parse_params_next(_Args, _SubParameter, _Index, OptionsMap, false, false) ->
    io:format("Unknown parameter~n"),
    {ok, OptionsMap}.
