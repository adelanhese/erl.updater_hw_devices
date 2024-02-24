-module(enetconfc_helpers).

%-------------------------------------------------------------------------------
% Helper functions for simplexml-structures
%-------------------------------------------------------------------------------
-export([
    simplexml_no_attributes/1,
    attributes_to_struct/2,
    simplexml_from_file/1,
    simplexml_to_file/2,

    simplexml_structure_to_xmlelement/1,
    simplexml_flatten_raw/1,
    simplexml_flatten_keys/1,
    simplexml_read_path/2
]).

%-------------------------------------------------------------------------------
% Helper functions for reading xml-files / xpath operations on them
%-------------------------------------------------------------------------------
-export([
    read_xml/1,
    read_xml_xpath/2,
    read_element_xpath/2,
    read_element_xpath_func/2
]).

% @doc Convert a simplexml-structure without attributes.
%
% Input:
%% ```
% A = [
%   {a,[],[
%       {b,[],[
%           {c1,[],["C1-Value"]},
%           {c2,[],["C2-Value"]},
%           {d,[],[
%               {d1,[],["Bla"]}
%           ]}
%       ]}
%   ]}
% ].
% [B] = A.
% enetconfc_helpers:simplexml_no_attributes(B).
% [
%  {a,[
%    {b,[
%      {c1,"C1-Value"},
%      {c2,"C2-Value"},
%      {d,[
%        {d1,"Bla"}
%      ]}
%    ]}
%  ]}
% ].
% '''
simplexml_no_attributes({Node, [], [Value]}) when is_list(Value) ->
    [{Node, Value}];
simplexml_no_attributes({Node, [], Children}) ->
    V = children_to_struct(Children, []),
    [{Node, V}];
simplexml_no_attributes({Node, _Attributes, Children}) ->
    %V = attributes_to_struct(Attributes, []) ++ children_to_struct(Children, []),
    V = children_to_struct(Children, []),
    [{Node, V}].

children_to_struct([], Acc) ->
    Acc;
children_to_struct([Value], Acc) when is_list(Value) ->
    Acc ++ [{"#text", Value}];
children_to_struct([Value | T], Acc) when is_tuple(Value) ->
    children_to_struct(T, Acc ++ simplexml_no_attributes(Value)).

attributes_to_struct([], Acc) ->
    Acc;
attributes_to_struct([{K, V} | T], Acc) when is_list(K) ->
    attributes_to_struct(T, Acc ++ [{"@" ++ K, V}]);
attributes_to_struct([{K, V} | T], Acc) when is_atom(K) ->
    attributes_to_struct(T, Acc ++ [{"@" ++ erlang:atom_to_list(K), V}]).

simplexml_structure_to_xmlelement(Structure) ->
    Xml = lists:flatten(xmerl:export_simple(Structure, xmerl_xml)),
    DataToWrite = erlang:list_to_bitstring(Xml),
    {Element, _} = xmerl_scan:string(erlang:bitstring_to_list(DataToWrite)),
    Element.

% @doc Write a simplexml-structure to file.
%
% Input:
%% ```
% A = [
%   {a,[],[
%       {b,[],[
%           {c1,[],["C1-Value"]},
%           {c2,[],["C2-Value"]},
%           {d,[],[
%               {d1,[],["Bla"]}
%           ]}
%       ]}
%   ]}
% ].
%
% enetconfc_helpers:simplexml_to_file("out.xml",A).
%
% Output:
% <?xml version="1.0"?>
% <a>
%   <b>
%     <c1>C1-Value</c1>
%     <c2>C2-Value</c2>
%     <d>
%       <d1>Bla</d1>
%     </d>
%   </b>
% </a>
% '''
simplexml_to_file(Filename, Data) ->
    Xml = lists:flatten(xmerl:export_simple(Data, xmerl_xml)),
    DataToWrite = erlang:list_to_bitstring(Xml),
    file:write_file(Filename, DataToWrite).

% @doc Read an xml-file and convert it to a simplexml-structure
%
% Input: out.xml
% ```
% <?xml version="1.0"?>
% <a>
%   <b>
%     <c1>C1-Value</c1>
%     <c2>C2-Value</c2>
%     <d>
%       <d1>Bla</d1>
%     </d>
%   </b>
% </a>
% '''
% Output:
% ```
% enetconfc_helpers:simplexml_from_file("out.xml").
%   {a,[],[
%       {b,[],[
%           {c1,[],["C1-Value"]},
%           {c2,[],["C2-Value"]},
%           {d,[],[
%               {d1,[],["Bla"]}
%           ]}
%       ]}
%   ]}.
% '''
simplexml_from_file(Filename) ->
    {Element, _} = xmerl_scan:file(Filename, [{space, normalize}]),
    [Clean] = xmerl_lib:remove_whitespace([Element]),
    xmerl_lib:simplify_element(Clean).

% @doc Read a path from a simplexml-structure
%
% This function is not as sofisticated as a full xpath-expression.
% But for simply getting a substructure or value it works fine.
%
% Example:
%% ```
% A = [
%   {a,[],[
%       {b,[],[
%           {c1,[],["C1-Value"]},
%           {c2,[],["C2-Value"]},
%           {d,[],[
%               {d1,[],["Bla"]}
%           ]}
%       ]}
%   ]}
% ].
% enetconfc_helpers:simplexml_read_path([a,b,c1],A).
% ["C1-Value"]
%% '''
simplexml_read_path([], Structure) ->
    Structure;
simplexml_read_path([H | T], Structure) ->
    {_, _, Content} = lists:keyfind(H, 1, Structure),
    simplexml_read_path(T, Content).

-spec read_xml(string()) -> term().
% @doc Reads an xml-file and returns the xml document()-structure
%
% The function assumes that a single root is stipulated in the file.
% If this is not the case - such as an xml-file with xml-fragments, then
% you need to use the {@link xmerl_scan:file/2. <em>xmerl_scan:file</em>} function yourself.
read_xml(Filename) ->
    {Element, _Rest} = xmerl_scan:file(Filename, [{space, normalize}]),
    Element.

% @doc Read an xml-file and apply an xpath-expression : return a list of simplified simplexml().
%
% The function assumes that a single root is stipulated in the file.
% If this is not the case - such as an xml-file with xml-fragments, then
% you need to use the xmerl_scan:file function yourself.
read_xml_xpath(Filename, Xpath) ->
    {Element, _Rest} = xmerl_scan:file(Filename, [{space, normalize}]),
    Items = xmerl_xpath:string(Xpath, Element),
    ItemsNoWhiteSpaces = xmerl_lib:remove_whitespace(Items),
    lists:map(fun(E) -> xmerl_lib:simplify_element(E) end, ItemsNoWhiteSpaces).

% @doc Read an document() structure and apply an xpath-expression
%
% It returns a list of simplified simplexml().
%
% The function assumes that the element contains a single-rooted document().
%
% Examples:
% <pre>
% NetworkSimpleXml =
% {network,[{xmlns,"http://ekinops.com/otns/config"}],[
%   {dcn,[],[
%     {ip,[],["1.2.3.4/24"]},
%     {gateway,[],["1.1.1.1"]}
%   ]},
%   {'dns-server-ips',[],["2.2.2.2"]},
%   {'ntp-server-ips',[],["3.3.3.3"]},
%   {'ntp-server-ips',[],["3.3.3.4"]},
%   {'ntp-server-ips',[],["3.3.3.5"]},
%   {'ntp-server-ips',[],["3.3.3.6"]},
%   {'ntp-server-ips',[],["3.3.3.7"]}
% ]}.
%
%Element = enetconfc_helpers:simplexml_structure_to_xmlelement([NetworkSimpleXml]).
%enetconfc_helpers:read_element_xpath(Element,"/network/dcn/ip/text()").
%["1.2.3.4/24"]
%enetconfc_helpers:read_element_xpath(Element,"/network/dcn/gateway/text()").
%["1.1.1.1"]
%enetconfc_helpers:read_element_xpath(Element,"/network/dns-server-ips/text()").
%["2.2.2.2"]
%enetconfc_helpers:read_element_xpath(Element,"/network/ntp-server-ips/text()").
%["3.3.3.3","3.3.3.4","3.3.3.5","3.3.3.6","3.3.3.7"]
% </pre>
%
read_element_xpath(Element, Xpath) ->
    Items = xmerl_xpath:string(Xpath, Element),
    ItemsNoWhiteSpaces = xmerl_lib:remove_whitespace(Items),
    lists:map(fun(E) -> xmerl_lib:simplify_element(E) end, ItemsNoWhiteSpaces).

% @doc Read an document() structure and apply an xpath-function-expression
%
% Examples:
%<pre>
% NetworkSimpleXml =
% {network,[{xmlns,"http://ekinops.com/otns/config"}],[
%   {dcn,[],[
%     {ip,[],["1.2.3.4/24"]},
%     {gateway,[],["1.1.1.1"]}
%   ]},
%   {'dns-server-ips',[],["2.2.2.2"]},
%   {'ntp-server-ips',[],["3.3.3.3"]},
%   {'ntp-server-ips',[],["3.3.3.4"]},
%   {'ntp-server-ips',[],["3.3.3.5"]},
%   {'ntp-server-ips',[],["3.3.3.6"]},
%   {'ntp-server-ips',[],["3.3.3.7"]}
% ]}.
%
%Element = enetconfc_helpers:simplexml_structure_to_xmlelement([NetworkSimpleXml]).
%enetconfc_helpers:read_element_xpath_func(Element,"boolean(count(/network/ntp-server-ips) > 2)").
%{xmlObj,boolean,true}
%
%enetconfc_helpers:read_element_xpath_func(Element,"boolean(count(/network/ntp-server-ips) > 5)").
%{xmlObj,boolean,false}
%
%enetconfc_helpers:read_element_xpath_func(Element,"boolean(count(/network/ntp-server-ips) > count(/network/dns-server-ips))").
%{xmlObj,boolean,true}
%
%enetconfc_helpers:read_element_xpath_func(Element,"boolean(count(/network/ntp-server-ips) = count(/network/dns-server-ips))").
%{xmlObj,boolean,false}
%
%enetconfc_helpers:read_element_xpath_func(Element,"count(/network/ntp-server-ips)").
%{xmlObj,number,5}
%
%enetconfc_helpers:read_element_xpath_func(Element,"concat(/network/dns-server-ips[1]/text(),'--',/network/ntp-server-ips[1])").
%{xmlObj,string,"2.2.2.2--3.3.3.3"}
%</pre>
%

read_element_xpath_func(Element, Xpath) ->
    xmerl_xpath:string(Xpath, Element).

% @doc This function will flatten the keys of a xmlsimple-structure
%
% Example:
%% ```
% A = [
%   {a,[],[
%       {b,[],[
%           {c1,[],["C1-Value"]},
%           {c2,[],["C2-Value"]},
%           {d,[],[
%               {d1,[],["Bla"]}
%           ]}
%       ]}
%   ]}
% ].
% enetconfc_helpers:simplexml_flatten_keys(A).
% [
%   {"b-d-d1","Bla"},
%   {"b-c2","C2-Value"},
%   {"b-c1","C1-Value"}
% ]
%% '''
simplexml_flatten_keys(SimpleXmlList) ->
    List = simplexml_flatten_raw(SimpleXmlList),
    [{atom_list_to_string(lists:droplast(Names)), Value} || {Names, Value} <- List].

% @doc This function will get a list of simple_xml() structures.
%
% The function will recurse into the xml and generate lists of names if the
% element is the context-list.
%
% This should simplify reading of mildly-structured data.
% The _raw - function indicates that the data is a list-names (atoms ) plus the
% value.
%
% Example:
%% ```
% A = [
%   {a,[],[
%       {b,[],[
%           {c1,[],["C1-Value"]},
%           {c2,[],["C2-Value"]},
%           {d,[],[
%               {d1,[],["Bla"]}
%           ]}
%       ]}
%   ]}
% ].
% enetconfc_helpers:simplexml_flatten_raw(A).
% [
%   {[d1,d,b,a],"Bla"},
%   {[c2,b,a],"C2-Value"},
%   {[c1,b,a],"C1-Value"}
% ]
%% '''
simplexml_flatten_raw([] = L) ->
    L;
simplexml_flatten_raw([H | T]) ->
    flattenme(H, T, [], []).

% ------------------------------------------------------------------------------
% Internal functions
% ------------------------------------------------------------------------------
% These took me a while to grasp.
flattenme([], [], _Path, Acc) ->
    Acc;
flattenme({Name, _, []}, [], Path, Acc) ->
    flattenme([], [], Path, [{[Name | Path], true} | Acc]);
flattenme({Name, _, []}, [H | T], Path, Acc) ->
    flattenme(H, T, Path, [{[Name | Path], true} | Acc]);
flattenme({Name, _, [Value]}, [], Path, Acc) when is_list(Value) ->
    flattenme([], [], Path, [{[Name | Path], Value} | Acc]);
flattenme({Name, _, [Value]}, [H | T], Path, Acc) when is_list(Value) ->
    flattenme(H, T, Path, [{[Name | Path], Value} | Acc]);
flattenme({Name, _, [H | T]}, [], Path, Acc) ->
    flattenme(H, T, [Name | Path], Acc);
flattenme({Name, _, [HV | TV]}, [H | T], Path, Acc) ->
    flattenme(HV, TV, [Name | Path], Acc) ++ flattenme(H, T, Path, []).

% @doc converts list of atoms into a string - concat with "-" ( reversed )
atom_list_to_string(List) ->
    Rev = lists:reverse(List),
    StrList = lists:map(fun(Atom) -> atom_to_list(Atom) end, Rev),
    string:join(StrList, "-").
