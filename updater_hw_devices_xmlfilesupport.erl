-module(updater_hw_devices_xmlfilesupport).
-include("updater_hw_devices_defines.hrl").

% Already tested and working
-export([read_xml_file/1,
         write_xml_file/2,
         read_xml_xpath/2,

         xml_file/5,
         xml_rd_file/3,
         xml_wr_file/4]).

read_xml_file(FilePath) ->
    {XmlDoc, _} = xmerl_scan:file(FilePath),
    Field = xmerl_xpath:string("//version", XmlDoc),
    {ok, Field}.
      
% Função para escrever em um campo específico de um arquivo XML
write_xml_file(FilePath, NewValue) ->
    {XmlDoc, _} = xmerl_scan:file(FilePath),
    NewXmlDoc = update_xml(XmlDoc, NewValue),
    {ok, NewXmlDoc}.

% Função auxiliar para atualizar o valor de um campo específico no documento XML
update_xml(XmlDoc, NewValue) ->
    NewXmlDoc = xmerl_xpath:set_xp("//version", fun(_) -> NewValue end, XmlDoc),
    NewXmlDoc.
%
% To read a device from indice:
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
%
%-----------------------------------------------------------------------------
-spec xml_file(string, string, string, string, [rd|wr]) -> {status, string}.
xml_file(XmlFileName, Sector, FieldIndex, NewFieldValue, rd) when (NewFieldValue == "") ->
    xml_rd_file(XmlFileName, Sector, FieldIndex);
xml_file(XmlFileName, Sector, FieldIndex, NewFieldValue, wr) ->
    xml_wr_file(XmlFileName, Sector, FieldIndex, NewFieldValue);
xml_file(_XmlFile, _Sector, _FieldIndex, _NewFieldValue, _RdWr) ->
    {error, "xml access error"}.

%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
xml_rd_file(XmlFileName, Sector, FieldIndex) ->
    {Field, Index} = split_field(FieldIndex),
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
    {ok, hd(Result)}.

 
%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
xml_wr_file(XmlFileName, Sector, FieldIndex, NewFieldValue) ->
    io:format("Not implemented: ~p, ~p, ~p, ~p, ~n", [XmlFileName, Sector, FieldIndex, NewFieldValue]),
    {error, "Write operation not implemented"}.


%-----------------------------------------------------------------------------
%
%
%-----------------------------------------------------------------------------
split_field(FieldIndex) ->
    IndexStr = string:substr(FieldIndex, length(FieldIndex), 1),
    {Index ,Result} = string:to_integer(IndexStr),
    split_field(Index, Result, FieldIndex).
split_field(Index, [], FieldIndex) ->
    Field = string:substr(FieldIndex, 1, length(FieldIndex)-1),
    {Field, Index};
split_field(error, no_integer, FieldIndex) ->
    {FieldIndex, -1};
split_field(_Val, _Result, FieldIndex) ->
    {FieldIndex, -1}.
