-module(updater_hw_devices_xmlfilesupport).
-include("updater_hw_devices_defines.hrl").

% Already tested and working
-export([read_xml_xpath/2,
         xml_file/5]).

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec xml_file(string, string, string, string, [rd|wr]) -> {status, string}.
xml_file(XmlFileName, Sector, FieldName, NewFieldValue, rd) when (NewFieldValue == "") ->
    xml_rd_file(XmlFileName, Sector, FieldName);
xml_file(XmlFileName, Sector, FieldName, NewFieldValue, wr) ->
    xml_wr_file(XmlFileName, Sector, FieldName, NewFieldValue);
xml_file(_XmlFile, _Sector, _FieldName, _NewFieldValue, _RdWr) ->
    {error, "xml access error"}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec xml_rd_file(string, string, string) -> {result, string}.
xml_rd_file(XmlFileName, Sector, FieldName) ->
    {Field, Index} = split_field(FieldName),
    xml_rd_file(XmlFileName, Sector, Field, Index).

xml_rd_file(XmlFileName, Sector, Field, Index) when (Index > 0) ->
    Xpath = unicode:characters_to_list(["/config/board[name='", Sector,"']/device[id='",integer_to_list(Index),"']/",Field,"/text()"]),
    Result = read_xml_xpath(XmlFileName, Xpath),
    xml_rd_file(Result);
xml_rd_file(XmlFileName, Sector, Field, _Index) ->
    Xpath = unicode:characters_to_list(["/config/board[name='", Sector,"']/",Field,"/text()"]),
    Result = read_xml_xpath(XmlFileName, Xpath),
    xml_rd_file(Result).

xml_rd_file([]) ->
    {ok, ""};
xml_rd_file(Result) ->
    {ok, updater_hw_devices_utils:list_to_string(Result, ",")}.

%-----------------------------------------------------------------------------
%
%  To search:
%
%        Sector:
%              <name>lc1</name>
%      
%        Id:
%             <id>2</id>
%      
%        Field:
%             <enabled>0</enabled>
%
%-----------------------------------------------------------------------------
-spec xml_wr_file(string, string, string, string) -> {result, string}.
xml_wr_file(XmlFileName, Sector, Field, NewValue) ->
    {Result, XmlData} = file:read_file(XmlFileName),
    xml_update_start_search({Result, XmlData}, XmlFileName, Sector, Field, NewValue).

xml_update_start_search({ok, XmlData}, XmlFileName, Sector, Field, NewValue) ->
    List = string:tokens(binary_to_list(XmlData), "\n"),
    {_, OldFieldValue} = xml_rd_file(XmlFileName, Sector, Field),
    {FieldStr, Id} = split_field(Field),
    SectorStr = unicode:characters_to_list(["<name>", Sector, "</name>"]),
    IdStr = unicode:characters_to_list(["<id>",integer_to_list(Id),"</id>"]),
    OldFieldStr = unicode:characters_to_list(["<",FieldStr,">",OldFieldValue,"</",FieldStr,">"]),
    {ResultSector, IndexSector} = xml_update_search_for_parameter(List, 1, SectorStr),
    {ResultId, IndexId} = xml_update_search_for_parameter(List, IndexSector, IdStr),
    {ResultField, IndexField} = xml_update_search_for_parameter(List, IndexId, OldFieldStr),
    xml_update_replace(ResultSector, ResultId, ResultField, List, IndexField, NewValue, XmlFileName);

xml_update_start_search({_Result, _XmlData}, _XmlFileName, _Sector, _Field, _NewFieldValue) ->
    {error, "Fail to access the xml file"}.

xml_update_replace(ok, ok, ok, List, Index, NewValue, XmlFileName) ->
    OldField = lists:nth(Index, List),
    NewField = updater_hw_devices_utils:replace_xml_field_value(OldField, NewValue),
    List1 = updater_hw_devices_utils:list_replace(List, Index, NewField),
    Text = lists:concat([io_lib:format("~s\n", [Element]) || Element <- List1]),
    Result = file:write_file(XmlFileName, Text),
    {Result, "Updating the field"};
xml_update_replace(_ResultSector, _ResultId, _ResultField, _List, _Index, _NewValue, _XmlFileName) ->
    {error, "Fail to replace the field"}.

%------------------------------------------------------------------------
%
% Loop to look for the "parameter" 
%
%------------------------------------------------------------------------
xml_update_search_for_parameter(List, Index, Parameter) when (Index < length(List)), (Index > 0) ->
    CurrentParameterPosition = string:str(lists:nth(Index, List), Parameter),
    xml_update_search_for_parameter_next(List, Index, Parameter, CurrentParameterPosition);
xml_update_search_for_parameter(_List, _Index, _Parameter) ->
    {error, -1}.
xml_update_search_for_parameter_next(List, Index, Parameter, 0) ->
    xml_update_search_for_parameter(List, Index + 1, Parameter);
xml_update_search_for_parameter_next(_List, Index, _Parameter, _CurrentParameterPosition) ->
    {ok, Index}.


% To read a device from indice:
%
%      ["config", "board", "device", "device"]
%                                                                                             [Sector]             Field
%      updater_hw_devices_xmlfilesupport:read_xml_xpath("etsc1_devices.xml","/config/board[name='fan']/device[id='2']/device/text()").
%      ["lpc55"]
%
%      updater_hw_devices_xmlfilesupport:read_xml_xpath("etsc1_devices.xml","/config/board[name='fan']/num_devices/text()").
%
%      updater_hw_devices_xmlfilesupport:xml_file("etsc1_devices.xml", "fan", "file1").
%
%      updater_hw_devices_xmlfilesupport:read_cfg_file("etsc1_devices.xml", "fan", "file1").
%
%      updater_hw_devices_xmlfilesupport:read_xml_xpath("etsc1_devices.xml","/config/board[name='lc1']/device[id='8']/vf_vr_ics_2ek_supported/id/text()").
%
%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
-spec read_xml_xpath(string, string) -> [value].
read_xml_xpath(Filename, Xpath) ->
    {Element, _Rest} = xmerl_scan:file(Filename, [{space, normalize}]),
    Items = xmerl_xpath:string(Xpath, Element),
    ItemsNoWhiteSpaces = xmerl_lib:remove_whitespace(Items),
    lists:map(fun(E) -> xmerl_lib:simplify_element(E) end, ItemsNoWhiteSpaces).

%-----------------------------------------------------------------------------
%
% Input:
%       <FieldName><Index>
%       ex: "enabled8"
%
% Output:
%       {FieldName, Index}
%       ex: {"enabled", "8"} 
%
% ToDo:
%       Currently the "index" is limited to max "9"
%       It will be necessary to fix it
%-----------------------------------------------------------------------------
split_field(FieldName) ->
    IndexStr = string:substr(FieldName, length(FieldName), 1),
    {Index ,Result} = string:to_integer(IndexStr),
    split_field(Index, Result, FieldName).
split_field(Index, [], FieldName) ->
    Field = string:substr(FieldName, 1, length(FieldName)-1),
    {Field, Index};
split_field(error, no_integer, FieldName) ->
    {FieldName, -1};
split_field(_Val, _Result, FieldName) ->
    {FieldName, -1}.
