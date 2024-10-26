-module(lustre@internals@patch).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([attribute_diff_to_json/2, is_empty_element_diff/1, element_diff_to_json/1, patch_to_json/1, attributes/2, elements/2]).
-export_type([patch/1, element_diff/1, attribute_diff/1]).

-type patch(ODT) :: {diff, element_diff(ODT)} |
    {emit, binary(), gleam@json:json()} |
    {init, list(binary()), lustre@internals@vdom:element(ODT)}.

-type element_diff(ODU) :: {element_diff,
        gleam@dict:dict(binary(), lustre@internals@vdom:element(ODU)),
        gleam@set:set(binary()),
        gleam@dict:dict(binary(), attribute_diff(ODU)),
        gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, ODU} |
            {error, list(gleam@dynamic:decode_error())}))}.

-type attribute_diff(ODV) :: {attribute_diff,
        gleam@set:set(lustre@internals@vdom:attribute(ODV)),
        gleam@set:set(binary()),
        gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, ODV} |
            {error, list(gleam@dynamic:decode_error())}))}.

-spec do_attribute(
    attribute_diff(OEU),
    binary(),
    {ok, lustre@internals@vdom:attribute(OEU)} | {error, nil},
    {ok, lustre@internals@vdom:attribute(OEU)} | {error, nil}
) -> attribute_diff(OEU).
do_attribute(Diff, Key, Old, New) ->
    case {Old, New} of
        {{error, _}, {error, _}} ->
            Diff;

        {{ok, Old@1}, {ok, {event, Name, Handler} = New@1}} when Old@1 =:= New@1 ->
            erlang:setelement(
                4,
                Diff,
                gleam@dict:insert(erlang:element(4, Diff), Name, Handler)
            );

        {{ok, Old@2}, {ok, New@2}} when Old@2 =:= New@2 ->
            Diff;

        {{ok, _}, {error, _}} ->
            erlang:setelement(
                3,
                Diff,
                gleam@set:insert(erlang:element(3, Diff), Key)
            );

        {_, {ok, {event, Name@1, Handler@1} = New@3}} ->
            erlang:setelement(
                4,
                erlang:setelement(
                    2,
                    Diff,
                    gleam@set:insert(erlang:element(2, Diff), New@3)
                ),
                gleam@dict:insert(erlang:element(4, Diff), Name@1, Handler@1)
            );

        {_, {ok, New@4}} ->
            erlang:setelement(
                2,
                Diff,
                gleam@set:insert(erlang:element(2, Diff), New@4)
            )
    end.

-spec do_key_sort(list(binary()), list(binary())) -> gleam@order:order().
do_key_sort(Xs, Ys) ->
    case {Xs, Ys} of
        {[], []} ->
            eq;

        {[], _} ->
            lt;

        {_, []} ->
            gt;

        {[<<"-"/utf8>> | Xs@1], [<<"-"/utf8>> | Ys@1]} ->
            do_key_sort(Xs@1, Ys@1);

        {[X | Xs@2], [Y | Ys@2]} ->
            _assert_subject = gleam@int:parse(X),
            {ok, X@1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre/internals/patch"/utf8>>,
                                function => <<"do_key_sort"/utf8>>,
                                line => 298})
            end,
            _assert_subject@1 = gleam@int:parse(Y),
            {ok, Y@1} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"lustre/internals/patch"/utf8>>,
                                function => <<"do_key_sort"/utf8>>,
                                line => 299})
            end,
            case gleam@int:compare(X@1, Y@1) of
                eq ->
                    do_key_sort(Xs@2, Ys@2);

                Order ->
                    Order
            end
    end.

-spec key_sort(binary(), binary()) -> gleam@order:order().
key_sort(X, Y) ->
    do_key_sort(
        gleam@string:split(X, <<"-"/utf8>>),
        gleam@string:split(Y, <<"-"/utf8>>)
    ).

-spec attribute_diff_to_json(attribute_diff(any()), binary()) -> gleam@json:json().
attribute_diff_to_json(Diff, Key) ->
    gleam@json:preprocessed_array(
        [gleam@json:preprocessed_array(
                (gleam@set:fold(
                    erlang:element(2, Diff),
                    [],
                    fun(Array, Attr) ->
                        case lustre@internals@vdom:attribute_to_json(Attr, Key) of
                            {ok, Json} ->
                                [Json | Array];

                            {error, _} ->
                                Array
                        end
                    end
                ))
            ),
            gleam@json:preprocessed_array(
                (gleam@set:fold(
                    erlang:element(3, Diff),
                    [],
                    fun(Array@1, Key@1) ->
                        [gleam@json:string(Key@1) | Array@1]
                    end
                ))
            )]
    ).

-spec zip(list(OFL), list(OFL)) -> list({gleam@option:option(OFL),
    gleam@option:option(OFL)}).
zip(Xs, Ys) ->
    case {Xs, Ys} of
        {[], []} ->
            [];

        {[X | Xs@1], [Y | Ys@1]} ->
            [{{some, X}, {some, Y}} | zip(Xs@1, Ys@1)];

        {[X@1 | Xs@2], []} ->
            [{{some, X@1}, none} | zip(Xs@2, [])];

        {[], [Y@1 | Ys@2]} ->
            [{none, {some, Y@1}} | zip([], Ys@2)]
    end.

-spec event_handler(lustre@internals@vdom:attribute(OFX)) -> {ok,
        {binary(),
            fun((gleam@dynamic:dynamic_()) -> {ok, OFX} |
                {error, list(gleam@dynamic:decode_error())})}} |
    {error, nil}.
event_handler(Attribute) ->
    case Attribute of
        {attribute, _, _, _} ->
            {error, nil};

        {event, Name, Handler} ->
            Name@1 = gleam@string:drop_left(Name, 2),
            {ok, {Name@1, Handler}}
    end.

-spec is_empty_element_diff(element_diff(any())) -> boolean().
is_empty_element_diff(Diff) ->
    ((erlang:element(2, Diff) =:= gleam@dict:new()) andalso (erlang:element(
        3,
        Diff
    )
    =:= gleam@set:new()))
    andalso (erlang:element(4, Diff) =:= gleam@dict:new()).

-spec is_empty_attribute_diff(attribute_diff(any())) -> boolean().
is_empty_attribute_diff(Diff) ->
    (erlang:element(2, Diff) =:= gleam@set:new()) andalso (erlang:element(
        3,
        Diff
    )
    =:= gleam@set:new()).

-spec element_diff_to_json(element_diff(any())) -> gleam@json:json().
element_diff_to_json(Diff) ->
    gleam@json:preprocessed_array(
        [gleam@json:preprocessed_array(
                lists:reverse(
                    begin
                        _pipe = maps:to_list(erlang:element(2, Diff)),
                        _pipe@1 = gleam@list:sort(
                            _pipe,
                            fun(X, Y) ->
                                key_sort(
                                    erlang:element(1, X),
                                    erlang:element(1, Y)
                                )
                            end
                        ),
                        gleam@list:fold(
                            _pipe@1,
                            [],
                            fun(Array, Patch) ->
                                {Key, Element} = Patch,
                                Json = gleam@json:preprocessed_array(
                                    [gleam@json:string(Key),
                                        lustre@internals@vdom:element_to_json(
                                            Element,
                                            Key
                                        )]
                                ),
                                [Json | Array]
                            end
                        )
                    end
                )
            ),
            gleam@json:preprocessed_array(
                begin
                    _pipe@2 = gleam@set:to_list(erlang:element(3, Diff)),
                    _pipe@3 = gleam@list:sort(_pipe@2, fun key_sort/2),
                    gleam@list:fold(
                        _pipe@3,
                        [],
                        fun(Array@1, Key@1) ->
                            Json@1 = gleam@json:preprocessed_array(
                                [gleam@json:string(Key@1)]
                            ),
                            [Json@1 | Array@1]
                        end
                    )
                end
            ),
            gleam@json:preprocessed_array(
                lists:reverse(
                    (gleam@dict:fold(
                        erlang:element(4, Diff),
                        [],
                        fun(Array@2, Key@2, Diff@1) ->
                            gleam@bool:guard(
                                is_empty_attribute_diff(Diff@1),
                                Array@2,
                                fun() ->
                                    Json@2 = gleam@json:preprocessed_array(
                                        [gleam@json:string(Key@2),
                                            attribute_diff_to_json(
                                                Diff@1,
                                                Key@2
                                            )]
                                    ),
                                    [Json@2 | Array@2]
                                end
                            )
                        end
                    ))
                )
            )]
    ).

-spec patch_to_json(patch(any())) -> gleam@json:json().
patch_to_json(Patch) ->
    case Patch of
        {diff, Diff} ->
            gleam@json:preprocessed_array(
                [gleam@json:int(0), element_diff_to_json(Diff)]
            );

        {emit, Name, Event} ->
            gleam@json:preprocessed_array(
                [gleam@json:int(1), gleam@json:string(Name), Event]
            );

        {init, Attrs, Element} ->
            gleam@json:preprocessed_array(
                [gleam@json:int(2),
                    gleam@json:array(Attrs, fun gleam@json:string/1),
                    lustre@internals@vdom:element_to_json(Element, <<"0"/utf8>>)]
            )
    end.

-spec attribute_dict(list(lustre@internals@vdom:attribute(OFR))) -> gleam@dict:dict(binary(), lustre@internals@vdom:attribute(OFR)).
attribute_dict(Attributes) ->
    gleam@list:fold(
        Attributes,
        gleam@dict:new(),
        fun(Dict, Attr) -> case Attr of
                {attribute, <<"class"/utf8>>, Value, _} ->
                    case gleam@dict:get(Dict, <<"class"/utf8>>) of
                        {ok, {attribute, _, Classes, _}} ->
                            Classes@1 = gleam@dynamic:from(
                                <<<<(lustre_escape_ffi:coerce(Classes))/binary,
                                        " "/utf8>>/binary,
                                    (lustre_escape_ffi:coerce(Value))/binary>>
                            ),
                            gleam@dict:insert(
                                Dict,
                                <<"class"/utf8>>,
                                {attribute, <<"class"/utf8>>, Classes@1, false}
                            );

                        {ok, _} ->
                            gleam@dict:insert(Dict, <<"class"/utf8>>, Attr);

                        {error, _} ->
                            gleam@dict:insert(Dict, <<"class"/utf8>>, Attr)
                    end;

                {attribute, <<"style"/utf8>>, Value@1, _} ->
                    case gleam@dict:get(Dict, <<"style"/utf8>>) of
                        {ok, {attribute, _, Styles, _}} ->
                            Styles@1 = gleam@dynamic:from(
                                lists:append(
                                    lustre_escape_ffi:coerce(Styles),
                                    lustre_escape_ffi:coerce(Value@1)
                                )
                            ),
                            gleam@dict:insert(
                                Dict,
                                <<"style"/utf8>>,
                                {attribute, <<"style"/utf8>>, Styles@1, false}
                            );

                        {ok, _} ->
                            gleam@dict:insert(Dict, <<"class"/utf8>>, Attr);

                        {error, _} ->
                            gleam@dict:insert(Dict, <<"class"/utf8>>, Attr)
                    end;

                {attribute, Key, _, _} ->
                    gleam@dict:insert(Dict, Key, Attr);

                {event, Key@1, _} ->
                    gleam@dict:insert(Dict, Key@1, Attr)
            end end
    ).

-spec attributes(
    list(lustre@internals@vdom:attribute(OEO)),
    list(lustre@internals@vdom:attribute(OEO))
) -> attribute_diff(OEO).
attributes(Old, New) ->
    Old@1 = attribute_dict(Old),
    New@1 = attribute_dict(New),
    Init = {attribute_diff, gleam@set:new(), gleam@set:new(), gleam@dict:new()},
    {Diff@2, New@4} = (gleam@dict:fold(
        Old@1,
        {Init, New@1},
        fun(_use0, Key, Attr) ->
            {Diff, New@2} = _use0,
            New_attr = gleam@dict:get(New@2, Key),
            Diff@1 = do_attribute(Diff, Key, {ok, Attr}, New_attr),
            New@3 = gleam@dict:delete(New@2, Key),
            {Diff@1, New@3}
        end
    )),
    gleam@dict:fold(
        New@4,
        Diff@2,
        fun(Diff@3, Key@1, Attr@1) ->
            do_attribute(Diff@3, Key@1, {error, nil}, {ok, Attr@1})
        end
    ).

-spec do_element_list(
    element_diff(OEH),
    list(lustre@internals@vdom:element(OEH)),
    list(lustre@internals@vdom:element(OEH)),
    binary()
) -> element_diff(OEH).
do_element_list(Diff, Old_elements, New_elements, Key) ->
    Children = zip(Old_elements, New_elements),
    gleam@list:index_fold(
        Children,
        Diff,
        fun(Diff@1, _use1, Pos) ->
            {Old, New} = _use1,
            Key@1 = <<<<Key/binary, "-"/utf8>>/binary,
                (gleam@int:to_string(Pos))/binary>>,
            do_elements(Diff@1, Old, New, Key@1)
        end
    ).

-spec do_elements(
    element_diff(OEA),
    gleam@option:option(lustre@internals@vdom:element(OEA)),
    gleam@option:option(lustre@internals@vdom:element(OEA)),
    binary()
) -> element_diff(OEA).
do_elements(Diff, Old, New, Key) ->
    case {Old, New} of
        {none, none} ->
            Diff;

        {{some, _}, none} ->
            erlang:setelement(
                3,
                Diff,
                gleam@set:insert(erlang:element(3, Diff), Key)
            );

        {none, {some, New@1}} ->
            erlang:setelement(
                5,
                erlang:setelement(
                    2,
                    Diff,
                    gleam@dict:insert(erlang:element(2, Diff), Key, New@1)
                ),
                fold_event_handlers(erlang:element(5, Diff), New@1, Key)
            );

        {{some, Old@1}, {some, New@2}} ->
            case {Old@1, New@2} of
                {{map, Old_subtree}, {map, New_subtree}} ->
                    do_elements(
                        Diff,
                        {some, Old_subtree()},
                        {some, New_subtree()},
                        Key
                    );

                {{map, Subtree}, _} ->
                    do_elements(Diff, {some, Subtree()}, {some, New@2}, Key);

                {_, {map, Subtree@1}} ->
                    do_elements(Diff, {some, Old@1}, {some, Subtree@1()}, Key);

                {{text, Old@2}, {text, New@3}} when Old@2 =:= New@3 ->
                    Diff;

                {{text, _}, {text, _}} ->
                    erlang:setelement(
                        2,
                        Diff,
                        gleam@dict:insert(erlang:element(2, Diff), Key, New@2)
                    );

                {{element, _, _, _, _, _, _, _}, {text, _}} ->
                    erlang:setelement(
                        2,
                        Diff,
                        gleam@dict:insert(erlang:element(2, Diff), Key, New@2)
                    );

                {{text, _}, {element, _, _, _, _, _, _, _}} ->
                    erlang:setelement(
                        5,
                        erlang:setelement(
                            2,
                            Diff,
                            gleam@dict:insert(
                                erlang:element(2, Diff),
                                Key,
                                New@2
                            )
                        ),
                        fold_event_handlers(erlang:element(5, Diff), New@2, Key)
                    );

                {{element, _, Old_ns, Old_tag, Old_attrs, Old_children, _, _},
                    {element, _, New_ns, New_tag, New_attrs, New_children, _, _}} when (Old_ns =:= New_ns) andalso (Old_tag =:= New_tag) ->
                    Attribute_diff = attributes(Old_attrs, New_attrs),
                    Handlers@1 = (gleam@dict:fold(
                        erlang:element(4, Attribute_diff),
                        erlang:element(5, Diff),
                        fun(Handlers, Name, Handler) ->
                            Name@1 = gleam@string:drop_left(Name, 2),
                            gleam@dict:insert(
                                Handlers,
                                <<<<Key/binary, "-"/utf8>>/binary,
                                    Name@1/binary>>,
                                Handler
                            )
                        end
                    )),
                    Diff@1 = erlang:setelement(
                        5,
                        erlang:setelement(
                            4,
                            Diff,
                            case is_empty_attribute_diff(Attribute_diff) of
                                true ->
                                    erlang:element(4, Diff);

                                false ->
                                    gleam@dict:insert(
                                        erlang:element(4, Diff),
                                        Key,
                                        Attribute_diff
                                    )
                            end
                        ),
                        Handlers@1
                    ),
                    do_element_list(Diff@1, Old_children, New_children, Key);

                {{element, _, _, _, _, _, _, _}, {element, _, _, _, _, _, _, _}} ->
                    erlang:setelement(
                        5,
                        erlang:setelement(
                            2,
                            Diff,
                            gleam@dict:insert(
                                erlang:element(2, Diff),
                                Key,
                                New@2
                            )
                        ),
                        fold_event_handlers(erlang:element(5, Diff), New@2, Key)
                    );

                {{fragment, Old_elements, _}, {fragment, New_elements, _}} ->
                    do_element_list(Diff, Old_elements, New_elements, Key);

                {_, {fragment, _, _}} ->
                    erlang:setelement(
                        5,
                        erlang:setelement(
                            2,
                            Diff,
                            gleam@dict:insert(
                                erlang:element(2, Diff),
                                Key,
                                New@2
                            )
                        ),
                        fold_event_handlers(erlang:element(5, Diff), New@2, Key)
                    );

                {{fragment, _, _}, _} ->
                    erlang:setelement(
                        5,
                        erlang:setelement(
                            2,
                            Diff,
                            gleam@dict:insert(
                                erlang:element(2, Diff),
                                Key,
                                New@2
                            )
                        ),
                        fold_event_handlers(erlang:element(5, Diff), New@2, Key)
                    )
            end
    end.

-spec elements(
    lustre@internals@vdom:element(ODW),
    lustre@internals@vdom:element(ODW)
) -> element_diff(ODW).
elements(Old, New) ->
    do_elements(
        {element_diff,
            gleam@dict:new(),
            gleam@set:new(),
            gleam@dict:new(),
            gleam@dict:new()},
        {some, Old},
        {some, New},
        <<"0"/utf8>>
    ).

-spec fold_element_list_event_handlers(
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, OGK} |
        {error, list(gleam@dynamic:decode_error())})),
    list(lustre@internals@vdom:element(OGK)),
    binary()
) -> gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, OGK} |
    {error, list(gleam@dynamic:decode_error())})).
fold_element_list_event_handlers(Handlers, Elements, Key) ->
    gleam@list:index_fold(
        Elements,
        Handlers,
        fun(Handlers@1, Element, Index) ->
            Key@1 = <<<<Key/binary, "-"/utf8>>/binary,
                (gleam@int:to_string(Index))/binary>>,
            fold_event_handlers(Handlers@1, Element, Key@1)
        end
    ).

-spec fold_event_handlers(
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, OGC} |
        {error, list(gleam@dynamic:decode_error())})),
    lustre@internals@vdom:element(OGC),
    binary()
) -> gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, OGC} |
    {error, list(gleam@dynamic:decode_error())})).
fold_event_handlers(Handlers, Element, Key) ->
    case Element of
        {text, _} ->
            Handlers;

        {map, Subtree} ->
            fold_event_handlers(Handlers, Subtree(), Key);

        {element, _, _, _, Attrs, Children, _, _} ->
            Handlers@2 = gleam@list:fold(
                Attrs,
                Handlers,
                fun(Handlers@1, Attr) -> case event_handler(Attr) of
                        {ok, {Name, Handler}} ->
                            gleam@dict:insert(
                                Handlers@1,
                                <<<<Key/binary, "-"/utf8>>/binary, Name/binary>>,
                                Handler
                            );

                        {error, _} ->
                            Handlers@1
                    end end
            ),
            fold_element_list_event_handlers(Handlers@2, Children, Key);

        {fragment, Elements, _} ->
            fold_element_list_event_handlers(Handlers, Elements, Key)
    end.
