-module(birl).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([time_of_day_to_string/1, time_of_day_to_short_string/1, to_unix/1, from_unix/1, to_unix_milli/1, from_unix_milli/1, to_unix_micro/1, from_unix_micro/1, compare/2, difference/2, add/2, subtract/2, weekday_to_string/1, weekday_to_short_string/1, range/3, set_timezone/2, get_timezone/1, set_offset/2, get_offset/1, parse_time_of_day/1, parse_naive_time_of_day/1, utc_now/0, now_with_offset/1, now_with_timezone/1, monotonic_now/0, to_date_string/1, to_naive_date_string/1, to_time_string/1, to_naive_time_string/1, to_iso8601/1, to_naive/1, month/1, string_month/1, short_string_month/1, get_day/1, get_time_of_day/1, to_erlang_datetime/1, to_erlang_universal_datetime/1, parse/1, from_naive/1, set_day/2, set_time_of_day/2, weekday/1, string_weekday/1, short_string_weekday/1, to_http/1, to_http_with_offset/1, now/0, from_erlang_local_datetime/1, from_erlang_universal_datetime/1, parse_relative/2, legible_difference/2, parse_weekday/1, from_http/1, parse_month/1]).
-export_type([time/0, day/0, time_of_day/0, weekday/0, month/0]).

-opaque time() :: {time,
        integer(),
        integer(),
        gleam@option:option(binary()),
        gleam@option:option(integer())}.

-type day() :: {day, integer(), integer(), integer()}.

-type time_of_day() :: {time_of_day, integer(), integer(), integer(), integer()}.

-type weekday() :: mon | tue | wed | thu | fri | sat | sun.

-type month() :: jan |
    feb |
    mar |
    apr |
    may |
    jun |
    jul |
    aug |
    sep |
    oct |
    nov |
    dec.

-spec time_of_day_to_string(time_of_day()) -> binary().
time_of_day_to_string(Value) ->
    <<<<<<<<<<<<(gleam@int:to_string(erlang:element(2, Value)))/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe = gleam@int:to_string(
                                erlang:element(3, Value)
                            ),
                            gleam@string:pad_left(_pipe, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@1 = gleam@int:to_string(erlang:element(4, Value)),
                    gleam@string:pad_left(_pipe@1, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "."/utf8>>/binary,
        (begin
            _pipe@2 = gleam@int:to_string(erlang:element(5, Value)),
            gleam@string:pad_left(_pipe@2, 3, <<"0"/utf8>>)
        end)/binary>>.

-spec time_of_day_to_short_string(time_of_day()) -> binary().
time_of_day_to_short_string(Value) ->
    <<<<(gleam@int:to_string(erlang:element(2, Value)))/binary, ":"/utf8>>/binary,
        (begin
            _pipe = gleam@int:to_string(erlang:element(3, Value)),
            gleam@string:pad_left(_pipe, 2, <<"0"/utf8>>)
        end)/binary>>.

-spec to_unix(time()) -> integer().
to_unix(Value) ->
    case Value of
        {time, T, _, _, _} ->
            T div 1000000
    end.

-spec from_unix(integer()) -> time().
from_unix(Value) ->
    {time, Value * 1000000, 0, none, none}.

-spec to_unix_milli(time()) -> integer().
to_unix_milli(Value) ->
    case Value of
        {time, T, _, _, _} ->
            T div 1000
    end.

-spec from_unix_milli(integer()) -> time().
from_unix_milli(Value) ->
    {time, Value * 1000, 0, none, none}.

-spec to_unix_micro(time()) -> integer().
to_unix_micro(Value) ->
    case Value of
        {time, T, _, _, _} ->
            T
    end.

-spec from_unix_micro(integer()) -> time().
from_unix_micro(Value) ->
    {time, Value, 0, none, none}.

-spec compare(time(), time()) -> gleam@order:order().
compare(A, B) ->
    {time, Wta, _, _, Mta} = A,
    {time, Wtb, _, _, Mtb} = B,
    {Ta@1, Tb@1} = case {Mta, Mtb} of
        {{some, Ta}, {some, Tb}} ->
            {Ta, Tb};

        {_, _} ->
            {Wta, Wtb}
    end,
    case {Ta@1 =:= Tb@1, Ta@1 < Tb@1} of
        {true, _} ->
            eq;

        {_, true} ->
            lt;

        {_, false} ->
            gt
    end.

-spec difference(time(), time()) -> birl@duration:duration().
difference(A, B) ->
    {time, Wta, _, _, Mta} = A,
    {time, Wtb, _, _, Mtb} = B,
    {Ta@1, Tb@1} = case {Mta, Mtb} of
        {{some, Ta}, {some, Tb}} ->
            {Ta, Tb};

        {_, _} ->
            {Wta, Wtb}
    end,
    {duration, Ta@1 - Tb@1}.

-spec add(time(), birl@duration:duration()) -> time().
add(Value, Duration) ->
    {time, Wt, O, Timezone, Mt} = Value,
    {duration, Duration@1} = Duration,
    case Mt of
        {some, Mt@1} ->
            {time, Wt + Duration@1, O, Timezone, {some, Mt@1 + Duration@1}};

        none ->
            {time, Wt + Duration@1, O, Timezone, none}
    end.

-spec subtract(time(), birl@duration:duration()) -> time().
subtract(Value, Duration) ->
    {time, Wt, O, Timezone, Mt} = Value,
    {duration, Duration@1} = Duration,
    case Mt of
        {some, Mt@1} ->
            {time, Wt - Duration@1, O, Timezone, {some, Mt@1 - Duration@1}};

        none ->
            {time, Wt - Duration@1, O, Timezone, none}
    end.

-spec weekday_to_string(weekday()) -> binary().
weekday_to_string(Value) ->
    case Value of
        mon ->
            <<"Monday"/utf8>>;

        tue ->
            <<"Tuesday"/utf8>>;

        wed ->
            <<"Wednesday"/utf8>>;

        thu ->
            <<"Thursday"/utf8>>;

        fri ->
            <<"Friday"/utf8>>;

        sat ->
            <<"Saturday"/utf8>>;

        sun ->
            <<"Sunday"/utf8>>
    end.

-spec weekday_to_short_string(weekday()) -> binary().
weekday_to_short_string(Value) ->
    case Value of
        mon ->
            <<"Mon"/utf8>>;

        tue ->
            <<"Tue"/utf8>>;

        wed ->
            <<"Wed"/utf8>>;

        thu ->
            <<"Thu"/utf8>>;

        fri ->
            <<"Fri"/utf8>>;

        sat ->
            <<"Sat"/utf8>>;

        sun ->
            <<"Sun"/utf8>>
    end.

-spec range(time(), gleam@option:option(time()), birl@duration:duration()) -> gleam@iterator:iterator(time()).
range(A, B, S) ->
    _assert_subject = case B of
        {some, B@1} ->
            (ranger:create(
                fun(_) -> true end,
                fun(Duration) ->
                    {duration, Value} = Duration,
                    {duration, -1 * Value}
                end,
                fun add/2,
                fun compare/2
            ))(A, B@1, S);

        none ->
            (ranger:create_infinite(
                fun(_) -> true end,
                fun add/2,
                fun compare/2
            ))(A, S)
    end,
    {ok, Range} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"range"/utf8>>,
                        line => 1145})
    end,
    Range.

-spec set_timezone(time(), binary()) -> {ok, time()} | {error, nil}.
set_timezone(Value, New_timezone) ->
    case gleam@list:key_find(
        [{<<"Africa/Abidjan"/utf8>>, 0},
            {<<"Africa/Algiers"/utf8>>, 3600},
            {<<"Africa/Bissau"/utf8>>, 0},
            {<<"Africa/Cairo"/utf8>>, 7200},
            {<<"Africa/Casablanca"/utf8>>, 3600},
            {<<"Africa/Ceuta"/utf8>>, 3600},
            {<<"Africa/El_Aaiun"/utf8>>, 3600},
            {<<"Africa/Johannesburg"/utf8>>, 7200},
            {<<"Africa/Juba"/utf8>>, 7200},
            {<<"Africa/Khartoum"/utf8>>, 7200},
            {<<"Africa/Lagos"/utf8>>, 3600},
            {<<"Africa/Maputo"/utf8>>, 7200},
            {<<"Africa/Monrovia"/utf8>>, 0},
            {<<"Africa/Nairobi"/utf8>>, 10800},
            {<<"Africa/Ndjamena"/utf8>>, 3600},
            {<<"Africa/Sao_Tome"/utf8>>, 0},
            {<<"Africa/Tripoli"/utf8>>, 7200},
            {<<"Africa/Tunis"/utf8>>, 3600},
            {<<"Africa/Windhoek"/utf8>>, 7200},
            {<<"America/Adak"/utf8>>, -36000},
            {<<"America/Anchorage"/utf8>>, -32400},
            {<<"America/Araguaina"/utf8>>, -10800},
            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
            {<<"America/Argentina/Salta"/utf8>>, -10800},
            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
            {<<"America/Asuncion"/utf8>>, -14400},
            {<<"America/Bahia"/utf8>>, -10800},
            {<<"America/Bahia_Banderas"/utf8>>, -21600},
            {<<"America/Barbados"/utf8>>, -14400},
            {<<"America/Belem"/utf8>>, -10800},
            {<<"America/Belize"/utf8>>, -21600},
            {<<"America/Boa_Vista"/utf8>>, -14400},
            {<<"America/Bogota"/utf8>>, -18000},
            {<<"America/Boise"/utf8>>, -25200},
            {<<"America/Cambridge_Bay"/utf8>>, -25200},
            {<<"America/Campo_Grande"/utf8>>, -14400},
            {<<"America/Cancun"/utf8>>, -18000},
            {<<"America/Caracas"/utf8>>, -14400},
            {<<"America/Cayenne"/utf8>>, -10800},
            {<<"America/Chicago"/utf8>>, -21600},
            {<<"America/Chihuahua"/utf8>>, -21600},
            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
            {<<"America/Costa_Rica"/utf8>>, -21600},
            {<<"America/Cuiaba"/utf8>>, -14400},
            {<<"America/Danmarkshavn"/utf8>>, 0},
            {<<"America/Dawson"/utf8>>, -25200},
            {<<"America/Dawson_Creek"/utf8>>, -25200},
            {<<"America/Denver"/utf8>>, -25200},
            {<<"America/Detroit"/utf8>>, -18000},
            {<<"America/Edmonton"/utf8>>, -25200},
            {<<"America/Eirunepe"/utf8>>, -18000},
            {<<"America/El_Salvador"/utf8>>, -21600},
            {<<"America/Fort_Nelson"/utf8>>, -25200},
            {<<"America/Fortaleza"/utf8>>, -10800},
            {<<"America/Glace_Bay"/utf8>>, -14400},
            {<<"America/Goose_Bay"/utf8>>, -14400},
            {<<"America/Grand_Turk"/utf8>>, -18000},
            {<<"America/Guatemala"/utf8>>, -21600},
            {<<"America/Guayaquil"/utf8>>, -18000},
            {<<"America/Guyana"/utf8>>, -14400},
            {<<"America/Halifax"/utf8>>, -14400},
            {<<"America/Havana"/utf8>>, -18000},
            {<<"America/Hermosillo"/utf8>>, -25200},
            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
            {<<"America/Indiana/Knox"/utf8>>, -21600},
            {<<"America/Indiana/Marengo"/utf8>>, -18000},
            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
            {<<"America/Indiana/Vevay"/utf8>>, -18000},
            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
            {<<"America/Indiana/Winamac"/utf8>>, -18000},
            {<<"America/Inuvik"/utf8>>, -25200},
            {<<"America/Iqaluit"/utf8>>, -18000},
            {<<"America/Jamaica"/utf8>>, -18000},
            {<<"America/Juneau"/utf8>>, -32400},
            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
            {<<"America/La_Paz"/utf8>>, -14400},
            {<<"America/Lima"/utf8>>, -18000},
            {<<"America/Los_Angeles"/utf8>>, -28800},
            {<<"America/Maceio"/utf8>>, -10800},
            {<<"America/Managua"/utf8>>, -21600},
            {<<"America/Manaus"/utf8>>, -14400},
            {<<"America/Martinique"/utf8>>, -14400},
            {<<"America/Matamoros"/utf8>>, -21600},
            {<<"America/Mazatlan"/utf8>>, -25200},
            {<<"America/Menominee"/utf8>>, -21600},
            {<<"America/Merida"/utf8>>, -21600},
            {<<"America/Metlakatla"/utf8>>, -32400},
            {<<"America/Mexico_City"/utf8>>, -21600},
            {<<"America/Miquelon"/utf8>>, -10800},
            {<<"America/Moncton"/utf8>>, -14400},
            {<<"America/Monterrey"/utf8>>, -21600},
            {<<"America/Montevideo"/utf8>>, -10800},
            {<<"America/New_York"/utf8>>, -18000},
            {<<"America/Nome"/utf8>>, -32400},
            {<<"America/Noronha"/utf8>>, -7200},
            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
            {<<"America/North_Dakota/Center"/utf8>>, -21600},
            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
            {<<"America/Nuuk"/utf8>>, -7200},
            {<<"America/Ojinaga"/utf8>>, -21600},
            {<<"America/Panama"/utf8>>, -18000},
            {<<"America/Paramaribo"/utf8>>, -10800},
            {<<"America/Phoenix"/utf8>>, -25200},
            {<<"America/Port-au-Prince"/utf8>>, -18000},
            {<<"America/Porto_Velho"/utf8>>, -14400},
            {<<"America/Puerto_Rico"/utf8>>, -14400},
            {<<"America/Punta_Arenas"/utf8>>, -10800},
            {<<"America/Rankin_Inlet"/utf8>>, -21600},
            {<<"America/Recife"/utf8>>, -10800},
            {<<"America/Regina"/utf8>>, -21600},
            {<<"America/Resolute"/utf8>>, -21600},
            {<<"America/Rio_Branco"/utf8>>, -18000},
            {<<"America/Santarem"/utf8>>, -10800},
            {<<"America/Santiago"/utf8>>, -14400},
            {<<"America/Santo_Domingo"/utf8>>, -14400},
            {<<"America/Sao_Paulo"/utf8>>, -10800},
            {<<"America/Scoresbysund"/utf8>>, -7200},
            {<<"America/Sitka"/utf8>>, -32400},
            {<<"America/St_Johns"/utf8>>, -12600},
            {<<"America/Swift_Current"/utf8>>, -21600},
            {<<"America/Tegucigalpa"/utf8>>, -21600},
            {<<"America/Thule"/utf8>>, -14400},
            {<<"America/Tijuana"/utf8>>, -28800},
            {<<"America/Toronto"/utf8>>, -18000},
            {<<"America/Vancouver"/utf8>>, -28800},
            {<<"America/Whitehorse"/utf8>>, -25200},
            {<<"America/Winnipeg"/utf8>>, -21600},
            {<<"America/Yakutat"/utf8>>, -32400},
            {<<"Antarctica/Casey"/utf8>>, 28800},
            {<<"Antarctica/Davis"/utf8>>, 25200},
            {<<"Antarctica/Macquarie"/utf8>>, 36000},
            {<<"Antarctica/Mawson"/utf8>>, 18000},
            {<<"Antarctica/Palmer"/utf8>>, -10800},
            {<<"Antarctica/Rothera"/utf8>>, -10800},
            {<<"Antarctica/Troll"/utf8>>, 0},
            {<<"Antarctica/Vostok"/utf8>>, 18000},
            {<<"Asia/Almaty"/utf8>>, 18000},
            {<<"Asia/Amman"/utf8>>, 10800},
            {<<"Asia/Anadyr"/utf8>>, 43200},
            {<<"Asia/Aqtau"/utf8>>, 18000},
            {<<"Asia/Aqtobe"/utf8>>, 18000},
            {<<"Asia/Ashgabat"/utf8>>, 18000},
            {<<"Asia/Atyrau"/utf8>>, 18000},
            {<<"Asia/Baghdad"/utf8>>, 10800},
            {<<"Asia/Baku"/utf8>>, 14400},
            {<<"Asia/Bangkok"/utf8>>, 25200},
            {<<"Asia/Barnaul"/utf8>>, 25200},
            {<<"Asia/Beirut"/utf8>>, 7200},
            {<<"Asia/Bishkek"/utf8>>, 21600},
            {<<"Asia/Chita"/utf8>>, 32400},
            {<<"Asia/Choibalsan"/utf8>>, 28800},
            {<<"Asia/Colombo"/utf8>>, 19800},
            {<<"Asia/Damascus"/utf8>>, 10800},
            {<<"Asia/Dhaka"/utf8>>, 21600},
            {<<"Asia/Dili"/utf8>>, 32400},
            {<<"Asia/Dubai"/utf8>>, 14400},
            {<<"Asia/Dushanbe"/utf8>>, 18000},
            {<<"Asia/Famagusta"/utf8>>, 7200},
            {<<"Asia/Gaza"/utf8>>, 7200},
            {<<"Asia/Hebron"/utf8>>, 7200},
            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
            {<<"Asia/Hong_Kong"/utf8>>, 28800},
            {<<"Asia/Hovd"/utf8>>, 25200},
            {<<"Asia/Irkutsk"/utf8>>, 28800},
            {<<"Asia/Jakarta"/utf8>>, 25200},
            {<<"Asia/Jayapura"/utf8>>, 32400},
            {<<"Asia/Jerusalem"/utf8>>, 7200},
            {<<"Asia/Kabul"/utf8>>, 16200},
            {<<"Asia/Kamchatka"/utf8>>, 43200},
            {<<"Asia/Karachi"/utf8>>, 18000},
            {<<"Asia/Kathmandu"/utf8>>, 20700},
            {<<"Asia/Khandyga"/utf8>>, 32400},
            {<<"Asia/Kolkata"/utf8>>, 19800},
            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
            {<<"Asia/Kuching"/utf8>>, 28800},
            {<<"Asia/Macau"/utf8>>, 28800},
            {<<"Asia/Magadan"/utf8>>, 39600},
            {<<"Asia/Makassar"/utf8>>, 28800},
            {<<"Asia/Manila"/utf8>>, 28800},
            {<<"Asia/Nicosia"/utf8>>, 7200},
            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
            {<<"Asia/Novosibirsk"/utf8>>, 25200},
            {<<"Asia/Omsk"/utf8>>, 21600},
            {<<"Asia/Oral"/utf8>>, 18000},
            {<<"Asia/Pontianak"/utf8>>, 25200},
            {<<"Asia/Pyongyang"/utf8>>, 32400},
            {<<"Asia/Qatar"/utf8>>, 10800},
            {<<"Asia/Qostanay"/utf8>>, 18000},
            {<<"Asia/Qyzylorda"/utf8>>, 18000},
            {<<"Asia/Riyadh"/utf8>>, 10800},
            {<<"Asia/Sakhalin"/utf8>>, 39600},
            {<<"Asia/Samarkand"/utf8>>, 18000},
            {<<"Asia/Seoul"/utf8>>, 32400},
            {<<"Asia/Shanghai"/utf8>>, 28800},
            {<<"Asia/Singapore"/utf8>>, 28800},
            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
            {<<"Asia/Taipei"/utf8>>, 28800},
            {<<"Asia/Tashkent"/utf8>>, 18000},
            {<<"Asia/Tbilisi"/utf8>>, 14400},
            {<<"Asia/Tehran"/utf8>>, 12600},
            {<<"Asia/Thimphu"/utf8>>, 21600},
            {<<"Asia/Tokyo"/utf8>>, 32400},
            {<<"Asia/Tomsk"/utf8>>, 25200},
            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
            {<<"Asia/Urumqi"/utf8>>, 21600},
            {<<"Asia/Ust-Nera"/utf8>>, 36000},
            {<<"Asia/Vladivostok"/utf8>>, 36000},
            {<<"Asia/Yakutsk"/utf8>>, 32400},
            {<<"Asia/Yangon"/utf8>>, 23400},
            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
            {<<"Asia/Yerevan"/utf8>>, 14400},
            {<<"Atlantic/Azores"/utf8>>, -3600},
            {<<"Atlantic/Bermuda"/utf8>>, -14400},
            {<<"Atlantic/Canary"/utf8>>, 0},
            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
            {<<"Atlantic/Faroe"/utf8>>, 0},
            {<<"Atlantic/Madeira"/utf8>>, 0},
            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
            {<<"Atlantic/Stanley"/utf8>>, -10800},
            {<<"Australia/Adelaide"/utf8>>, 34200},
            {<<"Australia/Brisbane"/utf8>>, 36000},
            {<<"Australia/Broken_Hill"/utf8>>, 34200},
            {<<"Australia/Darwin"/utf8>>, 34200},
            {<<"Australia/Eucla"/utf8>>, 31500},
            {<<"Australia/Hobart"/utf8>>, 36000},
            {<<"Australia/Lindeman"/utf8>>, 36000},
            {<<"Australia/Lord_Howe"/utf8>>, 37800},
            {<<"Australia/Melbourne"/utf8>>, 36000},
            {<<"Australia/Perth"/utf8>>, 28800},
            {<<"Australia/Sydney"/utf8>>, 36000},
            {<<"CET"/utf8>>, 3600},
            {<<"CST6CDT"/utf8>>, -21600},
            {<<"EET"/utf8>>, 7200},
            {<<"EST"/utf8>>, -18000},
            {<<"EST5EDT"/utf8>>, -18000},
            {<<"Etc/GMT"/utf8>>, 0},
            {<<"Etc/GMT+1"/utf8>>, -3600},
            {<<"Etc/GMT+10"/utf8>>, -36000},
            {<<"Etc/GMT+11"/utf8>>, -39600},
            {<<"Etc/GMT+12"/utf8>>, -43200},
            {<<"Etc/GMT+2"/utf8>>, -7200},
            {<<"Etc/GMT+3"/utf8>>, -10800},
            {<<"Etc/GMT+4"/utf8>>, -14400},
            {<<"Etc/GMT+5"/utf8>>, -18000},
            {<<"Etc/GMT+6"/utf8>>, -21600},
            {<<"Etc/GMT+7"/utf8>>, -25200},
            {<<"Etc/GMT+8"/utf8>>, -28800},
            {<<"Etc/GMT+9"/utf8>>, -32400},
            {<<"Etc/GMT-1"/utf8>>, 3600},
            {<<"Etc/GMT-10"/utf8>>, 36000},
            {<<"Etc/GMT-11"/utf8>>, 39600},
            {<<"Etc/GMT-12"/utf8>>, 43200},
            {<<"Etc/GMT-13"/utf8>>, 46800},
            {<<"Etc/GMT-14"/utf8>>, 50400},
            {<<"Etc/GMT-2"/utf8>>, 7200},
            {<<"Etc/GMT-3"/utf8>>, 10800},
            {<<"Etc/GMT-4"/utf8>>, 14400},
            {<<"Etc/GMT-5"/utf8>>, 18000},
            {<<"Etc/GMT-6"/utf8>>, 21600},
            {<<"Etc/GMT-7"/utf8>>, 25200},
            {<<"Etc/GMT-8"/utf8>>, 28800},
            {<<"Etc/GMT-9"/utf8>>, 32400},
            {<<"Etc/UTC"/utf8>>, 0},
            {<<"Europe/Andorra"/utf8>>, 3600},
            {<<"Europe/Astrakhan"/utf8>>, 14400},
            {<<"Europe/Athens"/utf8>>, 7200},
            {<<"Europe/Belgrade"/utf8>>, 3600},
            {<<"Europe/Berlin"/utf8>>, 3600},
            {<<"Europe/Brussels"/utf8>>, 3600},
            {<<"Europe/Bucharest"/utf8>>, 7200},
            {<<"Europe/Budapest"/utf8>>, 3600},
            {<<"Europe/Chisinau"/utf8>>, 7200},
            {<<"Europe/Dublin"/utf8>>, 3600},
            {<<"Europe/Gibraltar"/utf8>>, 3600},
            {<<"Europe/Helsinki"/utf8>>, 7200},
            {<<"Europe/Istanbul"/utf8>>, 10800},
            {<<"Europe/Kaliningrad"/utf8>>, 7200},
            {<<"Europe/Kirov"/utf8>>, 10800},
            {<<"Europe/Kyiv"/utf8>>, 7200},
            {<<"Europe/Lisbon"/utf8>>, 0},
            {<<"Europe/London"/utf8>>, 0},
            {<<"Europe/Madrid"/utf8>>, 3600},
            {<<"Europe/Malta"/utf8>>, 3600},
            {<<"Europe/Minsk"/utf8>>, 10800},
            {<<"Europe/Moscow"/utf8>>, 10800},
            {<<"Europe/Paris"/utf8>>, 3600},
            {<<"Europe/Prague"/utf8>>, 3600},
            {<<"Europe/Riga"/utf8>>, 7200},
            {<<"Europe/Rome"/utf8>>, 3600},
            {<<"Europe/Samara"/utf8>>, 14400},
            {<<"Europe/Saratov"/utf8>>, 14400},
            {<<"Europe/Simferopol"/utf8>>, 10800},
            {<<"Europe/Sofia"/utf8>>, 7200},
            {<<"Europe/Tallinn"/utf8>>, 7200},
            {<<"Europe/Tirane"/utf8>>, 3600},
            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
            {<<"Europe/Vienna"/utf8>>, 3600},
            {<<"Europe/Vilnius"/utf8>>, 7200},
            {<<"Europe/Volgograd"/utf8>>, 10800},
            {<<"Europe/Warsaw"/utf8>>, 3600},
            {<<"Europe/Zurich"/utf8>>, 3600},
            {<<"HST"/utf8>>, -36000},
            {<<"Indian/Chagos"/utf8>>, 21600},
            {<<"Indian/Maldives"/utf8>>, 18000},
            {<<"Indian/Mauritius"/utf8>>, 14400},
            {<<"MET"/utf8>>, 3600},
            {<<"MST"/utf8>>, -25200},
            {<<"MST7MDT"/utf8>>, -25200},
            {<<"PST8PDT"/utf8>>, -28800},
            {<<"Pacific/Apia"/utf8>>, 46800},
            {<<"Pacific/Auckland"/utf8>>, 43200},
            {<<"Pacific/Bougainville"/utf8>>, 39600},
            {<<"Pacific/Chatham"/utf8>>, 45900},
            {<<"Pacific/Easter"/utf8>>, -21600},
            {<<"Pacific/Efate"/utf8>>, 39600},
            {<<"Pacific/Fakaofo"/utf8>>, 46800},
            {<<"Pacific/Fiji"/utf8>>, 43200},
            {<<"Pacific/Galapagos"/utf8>>, -21600},
            {<<"Pacific/Gambier"/utf8>>, -32400},
            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
            {<<"Pacific/Guam"/utf8>>, 36000},
            {<<"Pacific/Honolulu"/utf8>>, -36000},
            {<<"Pacific/Kanton"/utf8>>, 46800},
            {<<"Pacific/Kiritimati"/utf8>>, 50400},
            {<<"Pacific/Kosrae"/utf8>>, 39600},
            {<<"Pacific/Kwajalein"/utf8>>, 43200},
            {<<"Pacific/Marquesas"/utf8>>, -34200},
            {<<"Pacific/Nauru"/utf8>>, 43200},
            {<<"Pacific/Niue"/utf8>>, -39600},
            {<<"Pacific/Norfolk"/utf8>>, 39600},
            {<<"Pacific/Noumea"/utf8>>, 39600},
            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
            {<<"Pacific/Palau"/utf8>>, 32400},
            {<<"Pacific/Pitcairn"/utf8>>, -28800},
            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
            {<<"Pacific/Rarotonga"/utf8>>, -36000},
            {<<"Pacific/Tahiti"/utf8>>, -36000},
            {<<"Pacific/Tarawa"/utf8>>, 43200},
            {<<"Pacific/Tongatapu"/utf8>>, 46800},
            {<<"WET"/utf8>>, 0}],
        New_timezone
    ) of
        {ok, New_offset_number} ->
            case Value of
                {time, T, _, _, Mt} ->
                    _pipe = {time,
                        T,
                        New_offset_number * 1000000,
                        {some, New_timezone},
                        Mt},
                    {ok, _pipe}
            end;

        {error, nil} ->
            {error, nil}
    end.

-spec get_timezone(time()) -> gleam@option:option(binary()).
get_timezone(Value) ->
    {time, _, _, Timezone, _} = Value,
    Timezone.

-spec parse_offset(binary()) -> {ok, integer()} | {error, nil}.
parse_offset(Offset) ->
    gleam@bool:guard(
        gleam@list:contains([<<"Z"/utf8>>, <<"z"/utf8>>], Offset),
        {ok, 0},
        fun() ->
            _assert_subject = gleam@regex:from_string(<<"([+-])"/utf8>>),
            {ok, Re} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"birl"/utf8>>,
                                function => <<"parse_offset"/utf8>>,
                                line => 1326})
            end,
            gleam@result:then(case gleam@regex:split(Re, Offset) of
                    [<<""/utf8>>, <<"+"/utf8>>, Offset@1] ->
                        {ok, {1, Offset@1}};

                    [<<""/utf8>>, <<"-"/utf8>>, Offset@2] ->
                        {ok, {-1, Offset@2}};

                    [_] ->
                        {ok, {1, Offset}};

                    _ ->
                        {error, nil}
                end, fun(_use0) ->
                    {Sign, Offset@3} = _use0,
                    case gleam@string:split(Offset@3, <<":"/utf8>>) of
                        [Hour_str, Minute_str] ->
                            gleam@result:then(
                                gleam@int:parse(Hour_str),
                                fun(Hour) ->
                                    gleam@result:then(
                                        gleam@int:parse(Minute_str),
                                        fun(Minute) ->
                                            {ok,
                                                ((Sign * ((Hour * 60) + Minute))
                                                * 60)
                                                * 1000000}
                                        end
                                    )
                                end
                            );

                        [Offset@4] ->
                            case gleam@string:length(Offset@4) of
                                1 ->
                                    gleam@result:then(
                                        gleam@int:parse(Offset@4),
                                        fun(Hour@1) ->
                                            {ok,
                                                ((Sign * Hour@1) * 3600) * 1000000}
                                        end
                                    );

                                2 ->
                                    gleam@result:then(
                                        gleam@int:parse(Offset@4),
                                        fun(Number) -> case Number < 14 of
                                                true ->
                                                    {ok,
                                                        ((Sign * Number) * 3600)
                                                        * 1000000};

                                                false ->
                                                    {ok,
                                                        ((Sign * (((Number div 10)
                                                        * 60)
                                                        + (Number rem 10)))
                                                        * 60)
                                                        * 1000000}
                                            end end
                                    );

                                3 ->
                                    _assert_subject@1 = gleam@string:first(
                                        Offset@4
                                    ),
                                    {ok, Hour_str@1} = case _assert_subject@1 of
                                        {ok, _} -> _assert_subject@1;
                                        _assert_fail@1 ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Assertion pattern match failed"/utf8>>,
                                                        value => _assert_fail@1,
                                                        module => <<"birl"/utf8>>,
                                                        function => <<"parse_offset"/utf8>>,
                                                        line => 1356}
                                                )
                                    end,
                                    Minute_str@1 = gleam@string:slice(
                                        Offset@4,
                                        1,
                                        2
                                    ),
                                    gleam@result:then(
                                        gleam@int:parse(Hour_str@1),
                                        fun(Hour@2) ->
                                            gleam@result:then(
                                                gleam@int:parse(Minute_str@1),
                                                fun(Minute@1) ->
                                                    {ok,
                                                        ((Sign * ((Hour@2 * 60)
                                                        + Minute@1))
                                                        * 60)
                                                        * 1000000}
                                                end
                                            )
                                        end
                                    );

                                4 ->
                                    Hour_str@2 = gleam@string:slice(
                                        Offset@4,
                                        0,
                                        2
                                    ),
                                    Minute_str@2 = gleam@string:slice(
                                        Offset@4,
                                        2,
                                        2
                                    ),
                                    gleam@result:then(
                                        gleam@int:parse(Hour_str@2),
                                        fun(Hour@3) ->
                                            gleam@result:then(
                                                gleam@int:parse(Minute_str@2),
                                                fun(Minute@2) ->
                                                    {ok,
                                                        ((Sign * ((Hour@3 * 60)
                                                        + Minute@2))
                                                        * 60)
                                                        * 1000000}
                                                end
                                            )
                                        end
                                    );

                                _ ->
                                    {error, nil}
                            end;

                        _ ->
                            {error, nil}
                    end
                end)
        end
    ).

-spec set_offset(time(), binary()) -> {ok, time()} | {error, nil}.
set_offset(Value, New_offset) ->
    gleam@result:then(
        parse_offset(New_offset),
        fun(New_offset_number) -> case Value of
                {time, T, _, Timezone, Mt} ->
                    _pipe = {time, T, New_offset_number, Timezone, Mt},
                    {ok, _pipe}
            end end
    ).

-spec generate_offset(integer()) -> {ok, binary()} | {error, nil}.
generate_offset(Offset) ->
    gleam@bool:guard(
        Offset =:= 0,
        {ok, <<"Z"/utf8>>},
        fun() ->
            case begin
                _pipe = [{Offset, micro_second}],
                _pipe@1 = birl@duration:new(_pipe),
                birl@duration:decompose(_pipe@1)
            end of
                [{Hour, hour}, {Minute, minute}] ->
                    _pipe@10 = [case Hour > 0 of
                            true ->
                                gleam@string:concat(
                                    [<<"+"/utf8>>,
                                        begin
                                            _pipe@2 = Hour,
                                            _pipe@3 = gleam@int:to_string(
                                                _pipe@2
                                            ),
                                            gleam@string:pad_left(
                                                _pipe@3,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                );

                            false ->
                                gleam@string:concat(
                                    [<<"-"/utf8>>,
                                        begin
                                            _pipe@4 = Hour,
                                            _pipe@5 = gleam@int:absolute_value(
                                                _pipe@4
                                            ),
                                            _pipe@6 = gleam@int:to_string(
                                                _pipe@5
                                            ),
                                            gleam@string:pad_left(
                                                _pipe@6,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                )
                        end, begin
                            _pipe@7 = Minute,
                            _pipe@8 = gleam@int:absolute_value(_pipe@7),
                            _pipe@9 = gleam@int:to_string(_pipe@8),
                            gleam@string:pad_left(_pipe@9, 2, <<"0"/utf8>>)
                        end],
                    _pipe@11 = gleam@string:join(_pipe@10, <<":"/utf8>>),
                    {ok, _pipe@11};

                [{Hour@1, hour}] ->
                    _pipe@17 = [case Hour@1 > 0 of
                            true ->
                                gleam@string:concat(
                                    [<<"+"/utf8>>,
                                        begin
                                            _pipe@12 = Hour@1,
                                            _pipe@13 = gleam@int:to_string(
                                                _pipe@12
                                            ),
                                            gleam@string:pad_left(
                                                _pipe@13,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                );

                            false ->
                                gleam@string:concat(
                                    [<<"-"/utf8>>,
                                        begin
                                            _pipe@14 = Hour@1,
                                            _pipe@15 = gleam@int:absolute_value(
                                                _pipe@14
                                            ),
                                            _pipe@16 = gleam@int:to_string(
                                                _pipe@15
                                            ),
                                            gleam@string:pad_left(
                                                _pipe@16,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end]
                                )
                        end, <<"00"/utf8>>],
                    _pipe@18 = gleam@string:join(_pipe@17, <<":"/utf8>>),
                    {ok, _pipe@18};

                _ ->
                    {error, nil}
            end
        end
    ).

-spec get_offset(time()) -> binary().
get_offset(Value) ->
    {time, _, Offset, _, _} = Value,
    _assert_subject = generate_offset(Offset),
    {ok, Offset@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"get_offset"/utf8>>,
                        line => 1202})
    end,
    Offset@1.

-spec is_invalid_date(binary()) -> boolean().
is_invalid_date(Date) ->
    _pipe = Date,
    _pipe@1 = gleam@string:to_utf_codepoints(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun gleam@string:utf_codepoint_to_int/1),
    gleam@list:any(_pipe@2, fun(Code) -> case Code of
                _ when Code =:= 45 ->
                    false;

                _ when (Code >= 48) andalso (Code =< 57) ->
                    false;

                _ ->
                    true
            end end).

-spec is_invalid_time(binary()) -> boolean().
is_invalid_time(Time) ->
    _pipe = Time,
    _pipe@1 = gleam@string:to_utf_codepoints(_pipe),
    _pipe@2 = gleam@list:map(_pipe@1, fun gleam@string:utf_codepoint_to_int/1),
    gleam@list:any(_pipe@2, fun(Code) -> case Code of
                _ when (Code >= 48) andalso (Code =< 58) ->
                    false;

                _ ->
                    true
            end end).

-spec parse_section(binary(), binary(), integer()) -> list({ok, integer()} |
    {error, nil}).
parse_section(Section, Pattern_string, Default) ->
    _assert_subject = gleam@regex:from_string(Pattern_string),
    {ok, Pattern} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"parse_section"/utf8>>,
                        line => 1521})
    end,
    case gleam@regex:scan(Pattern, Section) of
        [{match, _, [{some, Major}]}] ->
            [gleam@int:parse(Major), {ok, Default}, {ok, Default}];

        [{match, _, [{some, Major@1}, {some, Middle}]}] ->
            [gleam@int:parse(Major@1), gleam@int:parse(Middle), {ok, Default}];

        [{match, _, [{some, Major@2}, {some, Middle@1}, {some, Minor}]}] ->
            [gleam@int:parse(Major@2),
                gleam@int:parse(Middle@1),
                gleam@int:parse(Minor)];

        _ ->
            [{error, nil}]
    end.

-spec parse_date_section(binary()) -> {ok, list(integer())} | {error, nil}.
parse_date_section(Date) ->
    gleam@bool:guard(
        is_invalid_date(Date),
        {error, nil},
        fun() ->
            _pipe = case gleam_stdlib:contains_string(Date, <<"-"/utf8>>) of
                true ->
                    _assert_subject = gleam@regex:from_string(
                        <<"(\\d{4})(?:-(1[0-2]|0?[0-9]))?(?:-(3[0-1]|[1-2][0-9]|0?[0-9]))?"/utf8>>
                    ),
                    {ok, Dash_pattern} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail,
                                        module => <<"birl"/utf8>>,
                                        function => <<"parse_date_section"/utf8>>,
                                        line => 1441})
                    end,
                    case gleam@regex:scan(Dash_pattern, Date) of
                        [{match, _, [{some, Major}]}] ->
                            [gleam@int:parse(Major), {ok, 1}, {ok, 1}];

                        [{match, _, [{some, Major@1}, {some, Middle}]}] ->
                            [gleam@int:parse(Major@1),
                                gleam@int:parse(Middle),
                                {ok, 1}];

                        [{match,
                                _,
                                [{some, Major@2},
                                    {some, Middle@1},
                                    {some, Minor}]}] ->
                            [gleam@int:parse(Major@2),
                                gleam@int:parse(Middle@1),
                                gleam@int:parse(Minor)];

                        _ ->
                            [{error, nil}]
                    end;

                false ->
                    parse_section(
                        Date,
                        <<"(\\d{4})(1[0-2]|0?[0-9])?(3[0-1]|[1-2][0-9]|0?[0-9])?"/utf8>>,
                        1
                    )
            end,
            gleam@list:try_map(_pipe, fun gleam@function:identity/1)
        end
    ).

-spec parse_time_section(binary()) -> {ok, list(integer())} | {error, nil}.
parse_time_section(Time) ->
    gleam@bool:guard(
        is_invalid_time(Time),
        {error, nil},
        fun() ->
            _pipe = parse_section(
                Time,
                <<"(2[0-3]|1[0-9]|0?[0-9])([1-5][0-9]|0?[0-9])?([1-5][0-9]|0?[0-9])?"/utf8>>,
                0
            ),
            gleam@list:try_map(_pipe, fun gleam@function:identity/1)
        end
    ).

-spec parse_time_of_day(binary()) -> {ok, {time_of_day(), binary()}} |
    {error, nil}.
parse_time_of_day(Value) ->
    _assert_subject = gleam@regex:from_string(<<"(.*)([+|\\-].*)"/utf8>>),
    {ok, Offset_pattern} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"parse_time_of_day"/utf8>>,
                        line => 398})
    end,
    Time_string = case {gleam@string:starts_with(Value, <<"T"/utf8>>),
        gleam@string:starts_with(Value, <<"t"/utf8>>)} of
        {true, _} ->
            gleam@string:drop_left(Value, 1);

        {_, true} ->
            gleam@string:drop_left(Value, 1);

        {_, _} ->
            Value
    end,
    gleam@result:then(
        case gleam@string:ends_with(Time_string, <<"Z"/utf8>>) orelse gleam@string:ends_with(
            Time_string,
            <<"z"/utf8>>
        ) of
            true ->
                {ok, {gleam@string:drop_right(Value, 1), <<"+00:00"/utf8>>}};

            false ->
                case gleam@regex:scan(Offset_pattern, Value) of
                    [{match, _, [{some, Time_string@1}, {some, Offset_string}]}] ->
                        {ok, {Time_string@1, Offset_string}};

                    _ ->
                        {error, nil}
                end
        end,
        fun(_use0) ->
            {Time_string@2, Offset_string@1} = _use0,
            Time_string@3 = gleam@string:replace(
                Time_string@2,
                <<":"/utf8>>,
                <<""/utf8>>
            ),
            gleam@result:then(
                case {gleam@string:split(Time_string@3, <<"."/utf8>>),
                    gleam@string:split(Time_string@3, <<","/utf8>>)} of
                    {[_], [_]} ->
                        {ok, {Time_string@3, {ok, 0}}};

                    {[Time_string@4, Milli_seconds_string], [_]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_right(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam@int:parse(_pipe@2)
                                end}};

                    {[_], [Time_string@4, Milli_seconds_string]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_right(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam@int:parse(_pipe@2)
                                end}};

                    {_, _} ->
                        {error, nil}
                end,
                fun(_use0@1) ->
                    {Time_string@5, Milli_seconds_result} = _use0@1,
                    case Milli_seconds_result of
                        {ok, Milli_seconds} ->
                            gleam@result:then(
                                parse_time_section(Time_string@5),
                                fun(Time_of_day) ->
                                    [Hour, Minute, Second] = case Time_of_day of
                                        [_, _, _] -> Time_of_day;
                                        _assert_fail@1 ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Assertion pattern match failed"/utf8>>,
                                                        value => _assert_fail@1,
                                                        module => <<"birl"/utf8>>,
                                                        function => <<"parse_time_of_day"/utf8>>,
                                                        line => 447}
                                                )
                                    end,
                                    gleam@result:then(
                                        parse_offset(Offset_string@1),
                                        fun(Offset) ->
                                            gleam@result:then(
                                                generate_offset(Offset),
                                                fun(Offset_string@2) ->
                                                    {ok,
                                                        {{time_of_day,
                                                                Hour,
                                                                Minute,
                                                                Second,
                                                                Milli_seconds},
                                                            Offset_string@2}}
                                                end
                                            )
                                        end
                                    )
                                end
                            );

                        {error, nil} ->
                            {error, nil}
                    end
                end
            )
        end
    ).

-spec parse_naive_time_of_day(binary()) -> {ok, {time_of_day(), binary()}} |
    {error, nil}.
parse_naive_time_of_day(Value) ->
    Time_string = case {gleam@string:starts_with(Value, <<"T"/utf8>>),
        gleam@string:starts_with(Value, <<"t"/utf8>>)} of
        {true, _} ->
            gleam@string:drop_left(Value, 1);

        {_, true} ->
            gleam@string:drop_left(Value, 1);

        {_, _} ->
            Value
    end,
    Time_string@1 = gleam@string:replace(Time_string, <<":"/utf8>>, <<""/utf8>>),
    gleam@result:then(
        case {gleam@string:split(Time_string@1, <<"."/utf8>>),
            gleam@string:split(Time_string@1, <<","/utf8>>)} of
            {[_], [_]} ->
                {ok, {Time_string@1, {ok, 0}}};

            {[Time_string@2, Milli_seconds_string], [_]} ->
                {ok,
                    {Time_string@2,
                        begin
                            _pipe = Milli_seconds_string,
                            _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                            _pipe@2 = gleam@string:pad_right(
                                _pipe@1,
                                3,
                                <<"0"/utf8>>
                            ),
                            gleam@int:parse(_pipe@2)
                        end}};

            {[_], [Time_string@2, Milli_seconds_string]} ->
                {ok,
                    {Time_string@2,
                        begin
                            _pipe = Milli_seconds_string,
                            _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                            _pipe@2 = gleam@string:pad_right(
                                _pipe@1,
                                3,
                                <<"0"/utf8>>
                            ),
                            gleam@int:parse(_pipe@2)
                        end}};

            {_, _} ->
                {error, nil}
        end,
        fun(_use0) ->
            {Time_string@3, Milli_seconds_result} = _use0,
            case Milli_seconds_result of
                {ok, Milli_seconds} ->
                    gleam@result:then(
                        parse_time_section(Time_string@3),
                        fun(Time_of_day) ->
                            [Hour, Minute, Second] = case Time_of_day of
                                [_, _, _] -> Time_of_day;
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                value => _assert_fail,
                                                module => <<"birl"/utf8>>,
                                                function => <<"parse_naive_time_of_day"/utf8>>,
                                                line => 496})
                            end,
                            {ok,
                                {{time_of_day,
                                        Hour,
                                        Minute,
                                        Second,
                                        Milli_seconds},
                                    <<"Z"/utf8>>}}
                        end
                    );

                {error, nil} ->
                    {error, nil}
            end
        end
    ).

-spec weekday_from_int(integer()) -> {ok, weekday()} | {error, nil}.
weekday_from_int(Weekday) ->
    case Weekday of
        0 ->
            {ok, mon};

        1 ->
            {ok, tue};

        2 ->
            {ok, wed};

        3 ->
            {ok, thu};

        4 ->
            {ok, fri};

        5 ->
            {ok, sat};

        6 ->
            {ok, sun};

        _ ->
            {error, nil}
    end.

-spec month_from_int(integer()) -> {ok, month()} | {error, nil}.
month_from_int(Month) ->
    case Month of
        1 ->
            {ok, jan};

        2 ->
            {ok, feb};

        3 ->
            {ok, mar};

        4 ->
            {ok, apr};

        5 ->
            {ok, may};

        6 ->
            {ok, jun};

        7 ->
            {ok, jul};

        8 ->
            {ok, aug};

        9 ->
            {ok, sep};

        10 ->
            {ok, oct};

        11 ->
            {ok, nov};

        12 ->
            {ok, dec};

        _ ->
            {error, nil}
    end.

-spec utc_now() -> time().
utc_now() ->
    Now = birl_ffi:now(),
    Monotonic_now = birl_ffi:monotonic_now(),
    {time, Now, 0, {some, <<"Etc/UTC"/utf8>>}, {some, Monotonic_now}}.

-spec now_with_offset(binary()) -> {ok, time()} | {error, nil}.
now_with_offset(Offset) ->
    gleam@result:then(
        parse_offset(Offset),
        fun(Offset@1) ->
            Now = birl_ffi:now(),
            Monotonic_now = birl_ffi:monotonic_now(),
            _pipe = {time, Now, Offset@1, none, {some, Monotonic_now}},
            {ok, _pipe}
        end
    ).

-spec now_with_timezone(binary()) -> {ok, time()} | {error, nil}.
now_with_timezone(Timezone) ->
    case gleam@list:key_find(
        [{<<"Africa/Abidjan"/utf8>>, 0},
            {<<"Africa/Algiers"/utf8>>, 3600},
            {<<"Africa/Bissau"/utf8>>, 0},
            {<<"Africa/Cairo"/utf8>>, 7200},
            {<<"Africa/Casablanca"/utf8>>, 3600},
            {<<"Africa/Ceuta"/utf8>>, 3600},
            {<<"Africa/El_Aaiun"/utf8>>, 3600},
            {<<"Africa/Johannesburg"/utf8>>, 7200},
            {<<"Africa/Juba"/utf8>>, 7200},
            {<<"Africa/Khartoum"/utf8>>, 7200},
            {<<"Africa/Lagos"/utf8>>, 3600},
            {<<"Africa/Maputo"/utf8>>, 7200},
            {<<"Africa/Monrovia"/utf8>>, 0},
            {<<"Africa/Nairobi"/utf8>>, 10800},
            {<<"Africa/Ndjamena"/utf8>>, 3600},
            {<<"Africa/Sao_Tome"/utf8>>, 0},
            {<<"Africa/Tripoli"/utf8>>, 7200},
            {<<"Africa/Tunis"/utf8>>, 3600},
            {<<"Africa/Windhoek"/utf8>>, 7200},
            {<<"America/Adak"/utf8>>, -36000},
            {<<"America/Anchorage"/utf8>>, -32400},
            {<<"America/Araguaina"/utf8>>, -10800},
            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
            {<<"America/Argentina/Salta"/utf8>>, -10800},
            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
            {<<"America/Asuncion"/utf8>>, -14400},
            {<<"America/Bahia"/utf8>>, -10800},
            {<<"America/Bahia_Banderas"/utf8>>, -21600},
            {<<"America/Barbados"/utf8>>, -14400},
            {<<"America/Belem"/utf8>>, -10800},
            {<<"America/Belize"/utf8>>, -21600},
            {<<"America/Boa_Vista"/utf8>>, -14400},
            {<<"America/Bogota"/utf8>>, -18000},
            {<<"America/Boise"/utf8>>, -25200},
            {<<"America/Cambridge_Bay"/utf8>>, -25200},
            {<<"America/Campo_Grande"/utf8>>, -14400},
            {<<"America/Cancun"/utf8>>, -18000},
            {<<"America/Caracas"/utf8>>, -14400},
            {<<"America/Cayenne"/utf8>>, -10800},
            {<<"America/Chicago"/utf8>>, -21600},
            {<<"America/Chihuahua"/utf8>>, -21600},
            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
            {<<"America/Costa_Rica"/utf8>>, -21600},
            {<<"America/Cuiaba"/utf8>>, -14400},
            {<<"America/Danmarkshavn"/utf8>>, 0},
            {<<"America/Dawson"/utf8>>, -25200},
            {<<"America/Dawson_Creek"/utf8>>, -25200},
            {<<"America/Denver"/utf8>>, -25200},
            {<<"America/Detroit"/utf8>>, -18000},
            {<<"America/Edmonton"/utf8>>, -25200},
            {<<"America/Eirunepe"/utf8>>, -18000},
            {<<"America/El_Salvador"/utf8>>, -21600},
            {<<"America/Fort_Nelson"/utf8>>, -25200},
            {<<"America/Fortaleza"/utf8>>, -10800},
            {<<"America/Glace_Bay"/utf8>>, -14400},
            {<<"America/Goose_Bay"/utf8>>, -14400},
            {<<"America/Grand_Turk"/utf8>>, -18000},
            {<<"America/Guatemala"/utf8>>, -21600},
            {<<"America/Guayaquil"/utf8>>, -18000},
            {<<"America/Guyana"/utf8>>, -14400},
            {<<"America/Halifax"/utf8>>, -14400},
            {<<"America/Havana"/utf8>>, -18000},
            {<<"America/Hermosillo"/utf8>>, -25200},
            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
            {<<"America/Indiana/Knox"/utf8>>, -21600},
            {<<"America/Indiana/Marengo"/utf8>>, -18000},
            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
            {<<"America/Indiana/Vevay"/utf8>>, -18000},
            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
            {<<"America/Indiana/Winamac"/utf8>>, -18000},
            {<<"America/Inuvik"/utf8>>, -25200},
            {<<"America/Iqaluit"/utf8>>, -18000},
            {<<"America/Jamaica"/utf8>>, -18000},
            {<<"America/Juneau"/utf8>>, -32400},
            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
            {<<"America/La_Paz"/utf8>>, -14400},
            {<<"America/Lima"/utf8>>, -18000},
            {<<"America/Los_Angeles"/utf8>>, -28800},
            {<<"America/Maceio"/utf8>>, -10800},
            {<<"America/Managua"/utf8>>, -21600},
            {<<"America/Manaus"/utf8>>, -14400},
            {<<"America/Martinique"/utf8>>, -14400},
            {<<"America/Matamoros"/utf8>>, -21600},
            {<<"America/Mazatlan"/utf8>>, -25200},
            {<<"America/Menominee"/utf8>>, -21600},
            {<<"America/Merida"/utf8>>, -21600},
            {<<"America/Metlakatla"/utf8>>, -32400},
            {<<"America/Mexico_City"/utf8>>, -21600},
            {<<"America/Miquelon"/utf8>>, -10800},
            {<<"America/Moncton"/utf8>>, -14400},
            {<<"America/Monterrey"/utf8>>, -21600},
            {<<"America/Montevideo"/utf8>>, -10800},
            {<<"America/New_York"/utf8>>, -18000},
            {<<"America/Nome"/utf8>>, -32400},
            {<<"America/Noronha"/utf8>>, -7200},
            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
            {<<"America/North_Dakota/Center"/utf8>>, -21600},
            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
            {<<"America/Nuuk"/utf8>>, -7200},
            {<<"America/Ojinaga"/utf8>>, -21600},
            {<<"America/Panama"/utf8>>, -18000},
            {<<"America/Paramaribo"/utf8>>, -10800},
            {<<"America/Phoenix"/utf8>>, -25200},
            {<<"America/Port-au-Prince"/utf8>>, -18000},
            {<<"America/Porto_Velho"/utf8>>, -14400},
            {<<"America/Puerto_Rico"/utf8>>, -14400},
            {<<"America/Punta_Arenas"/utf8>>, -10800},
            {<<"America/Rankin_Inlet"/utf8>>, -21600},
            {<<"America/Recife"/utf8>>, -10800},
            {<<"America/Regina"/utf8>>, -21600},
            {<<"America/Resolute"/utf8>>, -21600},
            {<<"America/Rio_Branco"/utf8>>, -18000},
            {<<"America/Santarem"/utf8>>, -10800},
            {<<"America/Santiago"/utf8>>, -14400},
            {<<"America/Santo_Domingo"/utf8>>, -14400},
            {<<"America/Sao_Paulo"/utf8>>, -10800},
            {<<"America/Scoresbysund"/utf8>>, -7200},
            {<<"America/Sitka"/utf8>>, -32400},
            {<<"America/St_Johns"/utf8>>, -12600},
            {<<"America/Swift_Current"/utf8>>, -21600},
            {<<"America/Tegucigalpa"/utf8>>, -21600},
            {<<"America/Thule"/utf8>>, -14400},
            {<<"America/Tijuana"/utf8>>, -28800},
            {<<"America/Toronto"/utf8>>, -18000},
            {<<"America/Vancouver"/utf8>>, -28800},
            {<<"America/Whitehorse"/utf8>>, -25200},
            {<<"America/Winnipeg"/utf8>>, -21600},
            {<<"America/Yakutat"/utf8>>, -32400},
            {<<"Antarctica/Casey"/utf8>>, 28800},
            {<<"Antarctica/Davis"/utf8>>, 25200},
            {<<"Antarctica/Macquarie"/utf8>>, 36000},
            {<<"Antarctica/Mawson"/utf8>>, 18000},
            {<<"Antarctica/Palmer"/utf8>>, -10800},
            {<<"Antarctica/Rothera"/utf8>>, -10800},
            {<<"Antarctica/Troll"/utf8>>, 0},
            {<<"Antarctica/Vostok"/utf8>>, 18000},
            {<<"Asia/Almaty"/utf8>>, 18000},
            {<<"Asia/Amman"/utf8>>, 10800},
            {<<"Asia/Anadyr"/utf8>>, 43200},
            {<<"Asia/Aqtau"/utf8>>, 18000},
            {<<"Asia/Aqtobe"/utf8>>, 18000},
            {<<"Asia/Ashgabat"/utf8>>, 18000},
            {<<"Asia/Atyrau"/utf8>>, 18000},
            {<<"Asia/Baghdad"/utf8>>, 10800},
            {<<"Asia/Baku"/utf8>>, 14400},
            {<<"Asia/Bangkok"/utf8>>, 25200},
            {<<"Asia/Barnaul"/utf8>>, 25200},
            {<<"Asia/Beirut"/utf8>>, 7200},
            {<<"Asia/Bishkek"/utf8>>, 21600},
            {<<"Asia/Chita"/utf8>>, 32400},
            {<<"Asia/Choibalsan"/utf8>>, 28800},
            {<<"Asia/Colombo"/utf8>>, 19800},
            {<<"Asia/Damascus"/utf8>>, 10800},
            {<<"Asia/Dhaka"/utf8>>, 21600},
            {<<"Asia/Dili"/utf8>>, 32400},
            {<<"Asia/Dubai"/utf8>>, 14400},
            {<<"Asia/Dushanbe"/utf8>>, 18000},
            {<<"Asia/Famagusta"/utf8>>, 7200},
            {<<"Asia/Gaza"/utf8>>, 7200},
            {<<"Asia/Hebron"/utf8>>, 7200},
            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
            {<<"Asia/Hong_Kong"/utf8>>, 28800},
            {<<"Asia/Hovd"/utf8>>, 25200},
            {<<"Asia/Irkutsk"/utf8>>, 28800},
            {<<"Asia/Jakarta"/utf8>>, 25200},
            {<<"Asia/Jayapura"/utf8>>, 32400},
            {<<"Asia/Jerusalem"/utf8>>, 7200},
            {<<"Asia/Kabul"/utf8>>, 16200},
            {<<"Asia/Kamchatka"/utf8>>, 43200},
            {<<"Asia/Karachi"/utf8>>, 18000},
            {<<"Asia/Kathmandu"/utf8>>, 20700},
            {<<"Asia/Khandyga"/utf8>>, 32400},
            {<<"Asia/Kolkata"/utf8>>, 19800},
            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
            {<<"Asia/Kuching"/utf8>>, 28800},
            {<<"Asia/Macau"/utf8>>, 28800},
            {<<"Asia/Magadan"/utf8>>, 39600},
            {<<"Asia/Makassar"/utf8>>, 28800},
            {<<"Asia/Manila"/utf8>>, 28800},
            {<<"Asia/Nicosia"/utf8>>, 7200},
            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
            {<<"Asia/Novosibirsk"/utf8>>, 25200},
            {<<"Asia/Omsk"/utf8>>, 21600},
            {<<"Asia/Oral"/utf8>>, 18000},
            {<<"Asia/Pontianak"/utf8>>, 25200},
            {<<"Asia/Pyongyang"/utf8>>, 32400},
            {<<"Asia/Qatar"/utf8>>, 10800},
            {<<"Asia/Qostanay"/utf8>>, 18000},
            {<<"Asia/Qyzylorda"/utf8>>, 18000},
            {<<"Asia/Riyadh"/utf8>>, 10800},
            {<<"Asia/Sakhalin"/utf8>>, 39600},
            {<<"Asia/Samarkand"/utf8>>, 18000},
            {<<"Asia/Seoul"/utf8>>, 32400},
            {<<"Asia/Shanghai"/utf8>>, 28800},
            {<<"Asia/Singapore"/utf8>>, 28800},
            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
            {<<"Asia/Taipei"/utf8>>, 28800},
            {<<"Asia/Tashkent"/utf8>>, 18000},
            {<<"Asia/Tbilisi"/utf8>>, 14400},
            {<<"Asia/Tehran"/utf8>>, 12600},
            {<<"Asia/Thimphu"/utf8>>, 21600},
            {<<"Asia/Tokyo"/utf8>>, 32400},
            {<<"Asia/Tomsk"/utf8>>, 25200},
            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
            {<<"Asia/Urumqi"/utf8>>, 21600},
            {<<"Asia/Ust-Nera"/utf8>>, 36000},
            {<<"Asia/Vladivostok"/utf8>>, 36000},
            {<<"Asia/Yakutsk"/utf8>>, 32400},
            {<<"Asia/Yangon"/utf8>>, 23400},
            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
            {<<"Asia/Yerevan"/utf8>>, 14400},
            {<<"Atlantic/Azores"/utf8>>, -3600},
            {<<"Atlantic/Bermuda"/utf8>>, -14400},
            {<<"Atlantic/Canary"/utf8>>, 0},
            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
            {<<"Atlantic/Faroe"/utf8>>, 0},
            {<<"Atlantic/Madeira"/utf8>>, 0},
            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
            {<<"Atlantic/Stanley"/utf8>>, -10800},
            {<<"Australia/Adelaide"/utf8>>, 34200},
            {<<"Australia/Brisbane"/utf8>>, 36000},
            {<<"Australia/Broken_Hill"/utf8>>, 34200},
            {<<"Australia/Darwin"/utf8>>, 34200},
            {<<"Australia/Eucla"/utf8>>, 31500},
            {<<"Australia/Hobart"/utf8>>, 36000},
            {<<"Australia/Lindeman"/utf8>>, 36000},
            {<<"Australia/Lord_Howe"/utf8>>, 37800},
            {<<"Australia/Melbourne"/utf8>>, 36000},
            {<<"Australia/Perth"/utf8>>, 28800},
            {<<"Australia/Sydney"/utf8>>, 36000},
            {<<"CET"/utf8>>, 3600},
            {<<"CST6CDT"/utf8>>, -21600},
            {<<"EET"/utf8>>, 7200},
            {<<"EST"/utf8>>, -18000},
            {<<"EST5EDT"/utf8>>, -18000},
            {<<"Etc/GMT"/utf8>>, 0},
            {<<"Etc/GMT+1"/utf8>>, -3600},
            {<<"Etc/GMT+10"/utf8>>, -36000},
            {<<"Etc/GMT+11"/utf8>>, -39600},
            {<<"Etc/GMT+12"/utf8>>, -43200},
            {<<"Etc/GMT+2"/utf8>>, -7200},
            {<<"Etc/GMT+3"/utf8>>, -10800},
            {<<"Etc/GMT+4"/utf8>>, -14400},
            {<<"Etc/GMT+5"/utf8>>, -18000},
            {<<"Etc/GMT+6"/utf8>>, -21600},
            {<<"Etc/GMT+7"/utf8>>, -25200},
            {<<"Etc/GMT+8"/utf8>>, -28800},
            {<<"Etc/GMT+9"/utf8>>, -32400},
            {<<"Etc/GMT-1"/utf8>>, 3600},
            {<<"Etc/GMT-10"/utf8>>, 36000},
            {<<"Etc/GMT-11"/utf8>>, 39600},
            {<<"Etc/GMT-12"/utf8>>, 43200},
            {<<"Etc/GMT-13"/utf8>>, 46800},
            {<<"Etc/GMT-14"/utf8>>, 50400},
            {<<"Etc/GMT-2"/utf8>>, 7200},
            {<<"Etc/GMT-3"/utf8>>, 10800},
            {<<"Etc/GMT-4"/utf8>>, 14400},
            {<<"Etc/GMT-5"/utf8>>, 18000},
            {<<"Etc/GMT-6"/utf8>>, 21600},
            {<<"Etc/GMT-7"/utf8>>, 25200},
            {<<"Etc/GMT-8"/utf8>>, 28800},
            {<<"Etc/GMT-9"/utf8>>, 32400},
            {<<"Etc/UTC"/utf8>>, 0},
            {<<"Europe/Andorra"/utf8>>, 3600},
            {<<"Europe/Astrakhan"/utf8>>, 14400},
            {<<"Europe/Athens"/utf8>>, 7200},
            {<<"Europe/Belgrade"/utf8>>, 3600},
            {<<"Europe/Berlin"/utf8>>, 3600},
            {<<"Europe/Brussels"/utf8>>, 3600},
            {<<"Europe/Bucharest"/utf8>>, 7200},
            {<<"Europe/Budapest"/utf8>>, 3600},
            {<<"Europe/Chisinau"/utf8>>, 7200},
            {<<"Europe/Dublin"/utf8>>, 3600},
            {<<"Europe/Gibraltar"/utf8>>, 3600},
            {<<"Europe/Helsinki"/utf8>>, 7200},
            {<<"Europe/Istanbul"/utf8>>, 10800},
            {<<"Europe/Kaliningrad"/utf8>>, 7200},
            {<<"Europe/Kirov"/utf8>>, 10800},
            {<<"Europe/Kyiv"/utf8>>, 7200},
            {<<"Europe/Lisbon"/utf8>>, 0},
            {<<"Europe/London"/utf8>>, 0},
            {<<"Europe/Madrid"/utf8>>, 3600},
            {<<"Europe/Malta"/utf8>>, 3600},
            {<<"Europe/Minsk"/utf8>>, 10800},
            {<<"Europe/Moscow"/utf8>>, 10800},
            {<<"Europe/Paris"/utf8>>, 3600},
            {<<"Europe/Prague"/utf8>>, 3600},
            {<<"Europe/Riga"/utf8>>, 7200},
            {<<"Europe/Rome"/utf8>>, 3600},
            {<<"Europe/Samara"/utf8>>, 14400},
            {<<"Europe/Saratov"/utf8>>, 14400},
            {<<"Europe/Simferopol"/utf8>>, 10800},
            {<<"Europe/Sofia"/utf8>>, 7200},
            {<<"Europe/Tallinn"/utf8>>, 7200},
            {<<"Europe/Tirane"/utf8>>, 3600},
            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
            {<<"Europe/Vienna"/utf8>>, 3600},
            {<<"Europe/Vilnius"/utf8>>, 7200},
            {<<"Europe/Volgograd"/utf8>>, 10800},
            {<<"Europe/Warsaw"/utf8>>, 3600},
            {<<"Europe/Zurich"/utf8>>, 3600},
            {<<"HST"/utf8>>, -36000},
            {<<"Indian/Chagos"/utf8>>, 21600},
            {<<"Indian/Maldives"/utf8>>, 18000},
            {<<"Indian/Mauritius"/utf8>>, 14400},
            {<<"MET"/utf8>>, 3600},
            {<<"MST"/utf8>>, -25200},
            {<<"MST7MDT"/utf8>>, -25200},
            {<<"PST8PDT"/utf8>>, -28800},
            {<<"Pacific/Apia"/utf8>>, 46800},
            {<<"Pacific/Auckland"/utf8>>, 43200},
            {<<"Pacific/Bougainville"/utf8>>, 39600},
            {<<"Pacific/Chatham"/utf8>>, 45900},
            {<<"Pacific/Easter"/utf8>>, -21600},
            {<<"Pacific/Efate"/utf8>>, 39600},
            {<<"Pacific/Fakaofo"/utf8>>, 46800},
            {<<"Pacific/Fiji"/utf8>>, 43200},
            {<<"Pacific/Galapagos"/utf8>>, -21600},
            {<<"Pacific/Gambier"/utf8>>, -32400},
            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
            {<<"Pacific/Guam"/utf8>>, 36000},
            {<<"Pacific/Honolulu"/utf8>>, -36000},
            {<<"Pacific/Kanton"/utf8>>, 46800},
            {<<"Pacific/Kiritimati"/utf8>>, 50400},
            {<<"Pacific/Kosrae"/utf8>>, 39600},
            {<<"Pacific/Kwajalein"/utf8>>, 43200},
            {<<"Pacific/Marquesas"/utf8>>, -34200},
            {<<"Pacific/Nauru"/utf8>>, 43200},
            {<<"Pacific/Niue"/utf8>>, -39600},
            {<<"Pacific/Norfolk"/utf8>>, 39600},
            {<<"Pacific/Noumea"/utf8>>, 39600},
            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
            {<<"Pacific/Palau"/utf8>>, 32400},
            {<<"Pacific/Pitcairn"/utf8>>, -28800},
            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
            {<<"Pacific/Rarotonga"/utf8>>, -36000},
            {<<"Pacific/Tahiti"/utf8>>, -36000},
            {<<"Pacific/Tarawa"/utf8>>, 43200},
            {<<"Pacific/Tongatapu"/utf8>>, 46800},
            {<<"WET"/utf8>>, 0}],
        Timezone
    ) of
        {ok, Offset} ->
            Now = birl_ffi:now(),
            Monotonic_now = birl_ffi:monotonic_now(),
            _pipe = {time,
                Now,
                Offset * 1000000,
                {some, Timezone},
                {some, Monotonic_now}},
            {ok, _pipe};

        {error, nil} ->
            {error, nil}
    end.

-spec monotonic_now() -> integer().
monotonic_now() ->
    birl_ffi:monotonic_now().

-spec to_parts(time()) -> {{integer(), integer(), integer()},
    {integer(), integer(), integer(), integer()},
    binary()}.
to_parts(Value) ->
    case Value of
        {time, T, O, _, _} ->
            {Date, Time} = birl_ffi:to_parts(T, O),
            _assert_subject = generate_offset(O),
            {ok, Offset} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"birl"/utf8>>,
                                function => <<"to_parts"/utf8>>,
                                line => 1318})
            end,
            {Date, Time, Offset}
    end.

-spec to_date_string(time()) -> binary().
to_date_string(Value) ->
    {{Year, Month, Day}, _, Offset} = to_parts(Value),
    <<<<<<<<<<(gleam@int:to_string(Year))/binary, "-"/utf8>>/binary,
                    (begin
                        _pipe = Month,
                        _pipe@1 = gleam@int:to_string(_pipe),
                        gleam@string:pad_left(_pipe@1, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                "-"/utf8>>/binary,
            (begin
                _pipe@2 = Day,
                _pipe@3 = gleam@int:to_string(_pipe@2),
                gleam@string:pad_left(_pipe@3, 2, <<"0"/utf8>>)
            end)/binary>>/binary,
        Offset/binary>>.

-spec to_naive_date_string(time()) -> binary().
to_naive_date_string(Value) ->
    {{Year, Month, Day}, _, _} = to_parts(Value),
    <<<<<<<<(gleam@int:to_string(Year))/binary, "-"/utf8>>/binary,
                (begin
                    _pipe = Month,
                    _pipe@1 = gleam@int:to_string(_pipe),
                    gleam@string:pad_left(_pipe@1, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "-"/utf8>>/binary,
        (begin
            _pipe@2 = Day,
            _pipe@3 = gleam@int:to_string(_pipe@2),
            gleam@string:pad_left(_pipe@3, 2, <<"0"/utf8>>)
        end)/binary>>.

-spec to_time_string(time()) -> binary().
to_time_string(Value) ->
    {_, {Hour, Minute, Second, Milli_second}, Offset} = to_parts(Value),
    <<<<<<<<<<<<<<(begin
                                    _pipe = Hour,
                                    _pipe@1 = gleam@int:to_string(_pipe),
                                    gleam@string:pad_left(
                                        _pipe@1,
                                        2,
                                        <<"0"/utf8>>
                                    )
                                end)/binary,
                                ":"/utf8>>/binary,
                            (begin
                                _pipe@2 = Minute,
                                _pipe@3 = gleam@int:to_string(_pipe@2),
                                gleam@string:pad_left(_pipe@3, 2, <<"0"/utf8>>)
                            end)/binary>>/binary,
                        ":"/utf8>>/binary,
                    (begin
                        _pipe@4 = Second,
                        _pipe@5 = gleam@int:to_string(_pipe@4),
                        gleam@string:pad_left(_pipe@5, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                "."/utf8>>/binary,
            (begin
                _pipe@6 = Milli_second,
                _pipe@7 = gleam@int:to_string(_pipe@6),
                gleam@string:pad_left(_pipe@7, 3, <<"0"/utf8>>)
            end)/binary>>/binary,
        Offset/binary>>.

-spec to_naive_time_string(time()) -> binary().
to_naive_time_string(Value) ->
    {_, {Hour, Minute, Second, Milli_second}, _} = to_parts(Value),
    <<<<<<<<<<<<(begin
                                _pipe = Hour,
                                _pipe@1 = gleam@int:to_string(_pipe),
                                gleam@string:pad_left(_pipe@1, 2, <<"0"/utf8>>)
                            end)/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe@2 = Minute,
                            _pipe@3 = gleam@int:to_string(_pipe@2),
                            gleam@string:pad_left(_pipe@3, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@4 = Second,
                    _pipe@5 = gleam@int:to_string(_pipe@4),
                    gleam@string:pad_left(_pipe@5, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "."/utf8>>/binary,
        (begin
            _pipe@6 = Milli_second,
            _pipe@7 = gleam@int:to_string(_pipe@6),
            gleam@string:pad_left(_pipe@7, 3, <<"0"/utf8>>)
        end)/binary>>.

-spec to_iso8601(time()) -> binary().
to_iso8601(Value) ->
    {{Year, Month, Day}, {Hour, Minute, Second, Milli_second}, Offset} = to_parts(
        Value
    ),
    <<<<<<<<<<<<<<<<<<<<<<<<<<(gleam@int:to_string(Year))/binary, "-"/utf8>>/binary,
                                                    (begin
                                                        _pipe = Month,
                                                        _pipe@1 = gleam@int:to_string(
                                                            _pipe
                                                        ),
                                                        gleam@string:pad_left(
                                                            _pipe@1,
                                                            2,
                                                            <<"0"/utf8>>
                                                        )
                                                    end)/binary>>/binary,
                                                "-"/utf8>>/binary,
                                            (begin
                                                _pipe@2 = Day,
                                                _pipe@3 = gleam@int:to_string(
                                                    _pipe@2
                                                ),
                                                gleam@string:pad_left(
                                                    _pipe@3,
                                                    2,
                                                    <<"0"/utf8>>
                                                )
                                            end)/binary>>/binary,
                                        "T"/utf8>>/binary,
                                    (begin
                                        _pipe@4 = Hour,
                                        _pipe@5 = gleam@int:to_string(_pipe@4),
                                        gleam@string:pad_left(
                                            _pipe@5,
                                            2,
                                            <<"0"/utf8>>
                                        )
                                    end)/binary>>/binary,
                                ":"/utf8>>/binary,
                            (begin
                                _pipe@6 = Minute,
                                _pipe@7 = gleam@int:to_string(_pipe@6),
                                gleam@string:pad_left(_pipe@7, 2, <<"0"/utf8>>)
                            end)/binary>>/binary,
                        ":"/utf8>>/binary,
                    (begin
                        _pipe@8 = Second,
                        _pipe@9 = gleam@int:to_string(_pipe@8),
                        gleam@string:pad_left(_pipe@9, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                "."/utf8>>/binary,
            (begin
                _pipe@10 = Milli_second,
                _pipe@11 = gleam@int:to_string(_pipe@10),
                gleam@string:pad_left(_pipe@11, 3, <<"0"/utf8>>)
            end)/binary>>/binary,
        Offset/binary>>.

-spec to_naive(time()) -> binary().
to_naive(Value) ->
    {{Year, Month, Day}, {Hour, Minute, Second, Milli_second}, _} = to_parts(
        Value
    ),
    <<<<<<<<<<<<<<<<<<<<<<<<(gleam@int:to_string(Year))/binary, "-"/utf8>>/binary,
                                                (begin
                                                    _pipe = Month,
                                                    _pipe@1 = gleam@int:to_string(
                                                        _pipe
                                                    ),
                                                    gleam@string:pad_left(
                                                        _pipe@1,
                                                        2,
                                                        <<"0"/utf8>>
                                                    )
                                                end)/binary>>/binary,
                                            "-"/utf8>>/binary,
                                        (begin
                                            _pipe@2 = Day,
                                            _pipe@3 = gleam@int:to_string(
                                                _pipe@2
                                            ),
                                            gleam@string:pad_left(
                                                _pipe@3,
                                                2,
                                                <<"0"/utf8>>
                                            )
                                        end)/binary>>/binary,
                                    "T"/utf8>>/binary,
                                (begin
                                    _pipe@4 = Hour,
                                    _pipe@5 = gleam@int:to_string(_pipe@4),
                                    gleam@string:pad_left(
                                        _pipe@5,
                                        2,
                                        <<"0"/utf8>>
                                    )
                                end)/binary>>/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe@6 = Minute,
                            _pipe@7 = gleam@int:to_string(_pipe@6),
                            gleam@string:pad_left(_pipe@7, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@8 = Second,
                    _pipe@9 = gleam@int:to_string(_pipe@8),
                    gleam@string:pad_left(_pipe@9, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            "."/utf8>>/binary,
        (begin
            _pipe@10 = Milli_second,
            _pipe@11 = gleam@int:to_string(_pipe@10),
            gleam@string:pad_left(_pipe@11, 3, <<"0"/utf8>>)
        end)/binary>>.

-spec month(time()) -> month().
month(Value) ->
    {{_, Month, _}, _, _} = to_parts(Value),
    _assert_subject = month_from_int(Month),
    {ok, Month@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"month"/utf8>>,
                        line => 1099})
    end,
    Month@1.

-spec string_month(time()) -> binary().
string_month(Value) ->
    case month(Value) of
        jan ->
            <<"January"/utf8>>;

        feb ->
            <<"February"/utf8>>;

        mar ->
            <<"March"/utf8>>;

        apr ->
            <<"April"/utf8>>;

        may ->
            <<"May"/utf8>>;

        jun ->
            <<"June"/utf8>>;

        jul ->
            <<"July"/utf8>>;

        aug ->
            <<"August"/utf8>>;

        sep ->
            <<"September"/utf8>>;

        oct ->
            <<"October"/utf8>>;

        nov ->
            <<"November"/utf8>>;

        dec ->
            <<"December"/utf8>>
    end.

-spec short_string_month(time()) -> binary().
short_string_month(Value) ->
    case month(Value) of
        jan ->
            <<"Jan"/utf8>>;

        feb ->
            <<"Feb"/utf8>>;

        mar ->
            <<"Mar"/utf8>>;

        apr ->
            <<"Apr"/utf8>>;

        may ->
            <<"May"/utf8>>;

        jun ->
            <<"Jun"/utf8>>;

        jul ->
            <<"Jul"/utf8>>;

        aug ->
            <<"Aug"/utf8>>;

        sep ->
            <<"Sep"/utf8>>;

        oct ->
            <<"Oct"/utf8>>;

        nov ->
            <<"Nov"/utf8>>;

        dec ->
            <<"Dec"/utf8>>
    end.

-spec get_day(time()) -> day().
get_day(Value) ->
    {{Year, Month, Day}, _, _} = to_parts(Value),
    {day, Year, Month, Day}.

-spec get_time_of_day(time()) -> time_of_day().
get_time_of_day(Value) ->
    {_, {Hour, Minute, Second, Milli_second}, _} = to_parts(Value),
    {time_of_day, Hour, Minute, Second, Milli_second}.

-spec to_erlang_datetime(time()) -> {{integer(), integer(), integer()},
    {integer(), integer(), integer()}}.
to_erlang_datetime(Value) ->
    {Date, {Hour, Minute, Second, _}, _} = to_parts(Value),
    {Date, {Hour, Minute, Second}}.

-spec to_erlang_universal_datetime(time()) -> {{integer(), integer(), integer()},
    {integer(), integer(), integer()}}.
to_erlang_universal_datetime(Value) ->
    _assert_subject = set_offset(Value, <<"Z"/utf8>>),
    {ok, Value@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"to_erlang_universal_datetime"/utf8>>,
                        line => 1255})
    end,
    {Date, {Hour, Minute, Second, _}, _} = to_parts(Value@1),
    {Date, {Hour, Minute, Second}}.

-spec from_parts(
    {integer(), integer(), integer()},
    {integer(), integer(), integer(), integer()},
    binary()
) -> {ok, time()} | {error, nil}.
from_parts(Date, Time, Offset) ->
    gleam@result:then(
        parse_offset(Offset),
        fun(Offset_number) ->
            _pipe = birl_ffi:from_parts({Date, Time}, Offset_number),
            _pipe@1 = {time, _pipe, Offset_number, none, none},
            {ok, _pipe@1}
        end
    ).

-spec parse(binary()) -> {ok, time()} | {error, nil}.
parse(Value) ->
    _assert_subject = gleam@regex:from_string(<<"(.*)([+|\\-].*)"/utf8>>),
    {ok, Offset_pattern} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"parse"/utf8>>,
                        line => 295})
    end,
    Value@1 = gleam@string:trim(Value),
    gleam@result:then(
        case {gleam@string:split(Value@1, <<"T"/utf8>>),
            gleam@string:split(Value@1, <<"t"/utf8>>),
            gleam@string:split(Value@1, <<" "/utf8>>)} of
            {[Day_string, Time_string], _, _} ->
                {ok, {Day_string, Time_string}};

            {_, [Day_string, Time_string], _} ->
                {ok, {Day_string, Time_string}};

            {_, _, [Day_string, Time_string]} ->
                {ok, {Day_string, Time_string}};

            {[_], [_], [_]} ->
                {ok, {Value@1, <<"00"/utf8>>}};

            {_, _, _} ->
                {error, nil}
        end,
        fun(_use0) ->
            {Day_string@1, Offsetted_time_string} = _use0,
            Day_string@2 = gleam@string:trim(Day_string@1),
            Offsetted_time_string@1 = gleam@string:trim(Offsetted_time_string),
            gleam@result:then(
                case gleam@string:ends_with(
                    Offsetted_time_string@1,
                    <<"Z"/utf8>>
                )
                orelse gleam@string:ends_with(
                    Offsetted_time_string@1,
                    <<"z"/utf8>>
                ) of
                    true ->
                        {ok,
                            {Day_string@2,
                                gleam@string:drop_right(
                                    Offsetted_time_string@1,
                                    1
                                ),
                                <<"+00:00"/utf8>>}};

                    false ->
                        case gleam@regex:scan(
                            Offset_pattern,
                            Offsetted_time_string@1
                        ) of
                            [{match,
                                    _,
                                    [{some, Time_string@1},
                                        {some, Offset_string}]}] ->
                                {ok,
                                    {Day_string@2, Time_string@1, Offset_string}};

                            _ ->
                                case gleam@regex:scan(
                                    Offset_pattern,
                                    Day_string@2
                                ) of
                                    [{match,
                                            _,
                                            [{some, Day_string@3},
                                                {some, Offset_string@1}]}] ->
                                        {ok,
                                            {Day_string@3,
                                                <<"00"/utf8>>,
                                                Offset_string@1}};

                                    _ ->
                                        {error, nil}
                                end
                        end
                end,
                fun(_use0@1) ->
                    {Day_string@4, Time_string@2, Offset_string@2} = _use0@1,
                    Time_string@3 = gleam@string:replace(
                        Time_string@2,
                        <<":"/utf8>>,
                        <<""/utf8>>
                    ),
                    gleam@result:then(
                        case {gleam@string:split(Time_string@3, <<"."/utf8>>),
                            gleam@string:split(Time_string@3, <<","/utf8>>)} of
                            {[_], [_]} ->
                                {ok, {Time_string@3, {ok, 0}}};

                            {[Time_string@4, Milli_seconds_string], [_]} ->
                                {ok,
                                    {Time_string@4,
                                        begin
                                            _pipe = Milli_seconds_string,
                                            _pipe@1 = gleam@string:slice(
                                                _pipe,
                                                0,
                                                3
                                            ),
                                            _pipe@2 = gleam@string:pad_right(
                                                _pipe@1,
                                                3,
                                                <<"0"/utf8>>
                                            ),
                                            gleam@int:parse(_pipe@2)
                                        end}};

                            {[_], [Time_string@4, Milli_seconds_string]} ->
                                {ok,
                                    {Time_string@4,
                                        begin
                                            _pipe = Milli_seconds_string,
                                            _pipe@1 = gleam@string:slice(
                                                _pipe,
                                                0,
                                                3
                                            ),
                                            _pipe@2 = gleam@string:pad_right(
                                                _pipe@1,
                                                3,
                                                <<"0"/utf8>>
                                            ),
                                            gleam@int:parse(_pipe@2)
                                        end}};

                            {_, _} ->
                                {error, nil}
                        end,
                        fun(_use0@2) ->
                            {Time_string@5, Milli_seconds_result} = _use0@2,
                            case Milli_seconds_result of
                                {ok, Milli_seconds} ->
                                    gleam@result:then(
                                        parse_date_section(Day_string@4),
                                        fun(Day) ->
                                            [Year, Month, Date] = case Day of
                                                [_, _, _] -> Day;
                                                _assert_fail@1 ->
                                                    erlang:error(
                                                            #{gleam_error => let_assert,
                                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                                value => _assert_fail@1,
                                                                module => <<"birl"/utf8>>,
                                                                function => <<"parse"/utf8>>,
                                                                line => 363}
                                                        )
                                            end,
                                            gleam@result:then(
                                                parse_time_section(
                                                    Time_string@5
                                                ),
                                                fun(Time_of_day) ->
                                                    [Hour, Minute, Second] = case Time_of_day of
                                                        [_, _, _] -> Time_of_day;
                                                        _assert_fail@2 ->
                                                            erlang:error(
                                                                    #{gleam_error => let_assert,
                                                                        message => <<"Assertion pattern match failed"/utf8>>,
                                                                        value => _assert_fail@2,
                                                                        module => <<"birl"/utf8>>,
                                                                        function => <<"parse"/utf8>>,
                                                                        line => 366}
                                                                )
                                                    end,
                                                    from_parts(
                                                        {Year, Month, Date},
                                                        {Hour,
                                                            Minute,
                                                            Second,
                                                            Milli_seconds},
                                                        Offset_string@2
                                                    )
                                                end
                                            )
                                        end
                                    );

                                {error, nil} ->
                                    {error, nil}
                            end
                        end
                    )
                end
            )
        end
    ).

-spec from_naive(binary()) -> {ok, time()} | {error, nil}.
from_naive(Value) ->
    Value@1 = gleam@string:trim(Value),
    gleam@result:then(
        case {gleam@string:split(Value@1, <<"T"/utf8>>),
            gleam@string:split(Value@1, <<"t"/utf8>>),
            gleam@string:split(Value@1, <<" "/utf8>>)} of
            {[Day_string, Time_string], _, _} ->
                {ok, {Day_string, Time_string}};

            {_, [Day_string, Time_string], _} ->
                {ok, {Day_string, Time_string}};

            {_, _, [Day_string, Time_string]} ->
                {ok, {Day_string, Time_string}};

            {[_], [_], [_]} ->
                {ok, {Value@1, <<"00"/utf8>>}};

            {_, _, _} ->
                {error, nil}
        end,
        fun(_use0) ->
            {Day_string@1, Time_string@1} = _use0,
            Day_string@2 = gleam@string:trim(Day_string@1),
            Time_string@2 = gleam@string:trim(Time_string@1),
            Time_string@3 = gleam@string:replace(
                Time_string@2,
                <<":"/utf8>>,
                <<""/utf8>>
            ),
            gleam@result:then(
                case {gleam@string:split(Time_string@3, <<"."/utf8>>),
                    gleam@string:split(Time_string@3, <<","/utf8>>)} of
                    {[_], [_]} ->
                        {ok, {Time_string@3, {ok, 0}}};

                    {[Time_string@4, Milli_seconds_string], [_]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_right(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam@int:parse(_pipe@2)
                                end}};

                    {[_], [Time_string@4, Milli_seconds_string]} ->
                        {ok,
                            {Time_string@4,
                                begin
                                    _pipe = Milli_seconds_string,
                                    _pipe@1 = gleam@string:slice(_pipe, 0, 3),
                                    _pipe@2 = gleam@string:pad_right(
                                        _pipe@1,
                                        3,
                                        <<"0"/utf8>>
                                    ),
                                    gleam@int:parse(_pipe@2)
                                end}};

                    {_, _} ->
                        {error, nil}
                end,
                fun(_use0@1) ->
                    {Time_string@5, Milli_seconds_result} = _use0@1,
                    case Milli_seconds_result of
                        {ok, Milli_seconds} ->
                            gleam@result:then(
                                parse_date_section(Day_string@2),
                                fun(Day) ->
                                    [Year, Month, Date] = case Day of
                                        [_, _, _] -> Day;
                                        _assert_fail ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Assertion pattern match failed"/utf8>>,
                                                        value => _assert_fail,
                                                        module => <<"birl"/utf8>>,
                                                        function => <<"from_naive"/utf8>>,
                                                        line => 612}
                                                )
                                    end,
                                    gleam@result:then(
                                        parse_time_section(Time_string@5),
                                        fun(Time_of_day) ->
                                            [Hour, Minute, Second] = case Time_of_day of
                                                [_, _, _] -> Time_of_day;
                                                _assert_fail@1 ->
                                                    erlang:error(
                                                            #{gleam_error => let_assert,
                                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                                value => _assert_fail@1,
                                                                module => <<"birl"/utf8>>,
                                                                function => <<"from_naive"/utf8>>,
                                                                line => 615}
                                                        )
                                            end,
                                            from_parts(
                                                {Year, Month, Date},
                                                {Hour,
                                                    Minute,
                                                    Second,
                                                    Milli_seconds},
                                                <<"Z"/utf8>>
                                            )
                                        end
                                    )
                                end
                            );

                        {error, nil} ->
                            {error, nil}
                    end
                end
            )
        end
    ).

-spec set_day(time(), day()) -> time().
set_day(Value, Day) ->
    {_, Time, Offset} = to_parts(Value),
    {day, Year, Month, Date} = Day,
    _assert_subject = from_parts({Year, Month, Date}, Time, Offset),
    {ok, New_value} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"set_day"/utf8>>,
                        line => 1209})
    end,
    {time,
        erlang:element(2, New_value),
        erlang:element(3, New_value),
        erlang:element(4, Value),
        erlang:element(5, Value)}.

-spec set_time_of_day(time(), time_of_day()) -> time().
set_time_of_day(Value, Time) ->
    {Date, _, Offset} = to_parts(Value),
    {time_of_day, Hour, Minute, Second, Milli_second} = Time,
    _assert_subject = from_parts(
        Date,
        {Hour, Minute, Second, Milli_second},
        Offset
    ),
    {ok, New_value} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"set_time_of_day"/utf8>>,
                        line => 1227})
    end,
    {time,
        erlang:element(2, New_value),
        erlang:element(3, New_value),
        erlang:element(4, Value),
        erlang:element(5, Value)}.

-spec weekday(time()) -> weekday().
weekday(Value) ->
    case Value of
        {time, T, O, _, _} ->
            _assert_subject = weekday_from_int(birl_ffi:weekday(T, O)),
            {ok, Weekday} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"birl"/utf8>>,
                                function => <<"weekday"/utf8>>,
                                line => 1033})
            end,
            Weekday
    end.

-spec string_weekday(time()) -> binary().
string_weekday(Value) ->
    _pipe = weekday(Value),
    weekday_to_string(_pipe).

-spec short_string_weekday(time()) -> binary().
short_string_weekday(Value) ->
    _pipe = weekday(Value),
    weekday_to_short_string(_pipe).

-spec to_http(time()) -> binary().
to_http(Value) ->
    _assert_subject = set_offset(Value, <<"Z"/utf8>>),
    {ok, Value@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"to_http"/utf8>>,
                        line => 630})
    end,
    {{Year, _, Day}, {Hour, Minute, Second, _}, _} = to_parts(Value@1),
    Short_weekday = short_string_weekday(Value@1),
    Short_month = short_string_month(Value@1),
    <<<<<<<<<<<<<<<<<<<<<<<<<<Short_weekday/binary, ", "/utf8>>/binary,
                                                    (begin
                                                        _pipe = Day,
                                                        _pipe@1 = gleam@int:to_string(
                                                            _pipe
                                                        ),
                                                        gleam@string:pad_left(
                                                            _pipe@1,
                                                            2,
                                                            <<"0"/utf8>>
                                                        )
                                                    end)/binary>>/binary,
                                                " "/utf8>>/binary,
                                            Short_month/binary>>/binary,
                                        " "/utf8>>/binary,
                                    (gleam@int:to_string(Year))/binary>>/binary,
                                " "/utf8>>/binary,
                            (begin
                                _pipe@2 = Hour,
                                _pipe@3 = gleam@int:to_string(_pipe@2),
                                gleam@string:pad_left(_pipe@3, 2, <<"0"/utf8>>)
                            end)/binary>>/binary,
                        ":"/utf8>>/binary,
                    (begin
                        _pipe@4 = Minute,
                        _pipe@5 = gleam@int:to_string(_pipe@4),
                        gleam@string:pad_left(_pipe@5, 2, <<"0"/utf8>>)
                    end)/binary>>/binary,
                ":"/utf8>>/binary,
            (begin
                _pipe@6 = Second,
                _pipe@7 = gleam@int:to_string(_pipe@6),
                gleam@string:pad_left(_pipe@7, 2, <<"0"/utf8>>)
            end)/binary>>/binary,
        " GMT"/utf8>>.

-spec to_http_with_offset(time()) -> binary().
to_http_with_offset(Value) ->
    {{Year, _, Day}, {Hour, Minute, Second, _}, Offset} = to_parts(Value),
    Short_weekday = short_string_weekday(Value),
    Short_month = short_string_month(Value),
    Offset@1 = case Offset of
        <<"Z"/utf8>> ->
            <<"GMT"/utf8>>;

        _ ->
            Offset
    end,
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<Short_weekday/binary, ", "/utf8>>/binary,
                                                        (begin
                                                            _pipe = Day,
                                                            _pipe@1 = gleam@int:to_string(
                                                                _pipe
                                                            ),
                                                            gleam@string:pad_left(
                                                                _pipe@1,
                                                                2,
                                                                <<"0"/utf8>>
                                                            )
                                                        end)/binary>>/binary,
                                                    " "/utf8>>/binary,
                                                Short_month/binary>>/binary,
                                            " "/utf8>>/binary,
                                        (gleam@int:to_string(Year))/binary>>/binary,
                                    " "/utf8>>/binary,
                                (begin
                                    _pipe@2 = Hour,
                                    _pipe@3 = gleam@int:to_string(_pipe@2),
                                    gleam@string:pad_left(
                                        _pipe@3,
                                        2,
                                        <<"0"/utf8>>
                                    )
                                end)/binary>>/binary,
                            ":"/utf8>>/binary,
                        (begin
                            _pipe@4 = Minute,
                            _pipe@5 = gleam@int:to_string(_pipe@4),
                            gleam@string:pad_left(_pipe@5, 2, <<"0"/utf8>>)
                        end)/binary>>/binary,
                    ":"/utf8>>/binary,
                (begin
                    _pipe@6 = Second,
                    _pipe@7 = gleam@int:to_string(_pipe@6),
                    gleam@string:pad_left(_pipe@7, 2, <<"0"/utf8>>)
                end)/binary>>/binary,
            " "/utf8>>/binary,
        Offset@1/binary>>.

-spec now() -> time().
now() ->
    Now = birl_ffi:now(),
    Offset_in_minutes = birl_ffi:local_offset(),
    Monotonic_now = birl_ffi:monotonic_now(),
    Timezone = birl_ffi:local_timezone(),
    {time,
        Now,
        Offset_in_minutes * 60000000,
        begin
            _pipe = gleam@option:map(
                Timezone,
                fun(Tz) ->
                    case gleam@list:any(
                        [{<<"Africa/Abidjan"/utf8>>, 0},
                            {<<"Africa/Algiers"/utf8>>, 3600},
                            {<<"Africa/Bissau"/utf8>>, 0},
                            {<<"Africa/Cairo"/utf8>>, 7200},
                            {<<"Africa/Casablanca"/utf8>>, 3600},
                            {<<"Africa/Ceuta"/utf8>>, 3600},
                            {<<"Africa/El_Aaiun"/utf8>>, 3600},
                            {<<"Africa/Johannesburg"/utf8>>, 7200},
                            {<<"Africa/Juba"/utf8>>, 7200},
                            {<<"Africa/Khartoum"/utf8>>, 7200},
                            {<<"Africa/Lagos"/utf8>>, 3600},
                            {<<"Africa/Maputo"/utf8>>, 7200},
                            {<<"Africa/Monrovia"/utf8>>, 0},
                            {<<"Africa/Nairobi"/utf8>>, 10800},
                            {<<"Africa/Ndjamena"/utf8>>, 3600},
                            {<<"Africa/Sao_Tome"/utf8>>, 0},
                            {<<"Africa/Tripoli"/utf8>>, 7200},
                            {<<"Africa/Tunis"/utf8>>, 3600},
                            {<<"Africa/Windhoek"/utf8>>, 7200},
                            {<<"America/Adak"/utf8>>, -36000},
                            {<<"America/Anchorage"/utf8>>, -32400},
                            {<<"America/Araguaina"/utf8>>, -10800},
                            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
                            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
                            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
                            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
                            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
                            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
                            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
                            {<<"America/Argentina/Salta"/utf8>>, -10800},
                            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
                            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
                            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
                            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
                            {<<"America/Asuncion"/utf8>>, -14400},
                            {<<"America/Bahia"/utf8>>, -10800},
                            {<<"America/Bahia_Banderas"/utf8>>, -21600},
                            {<<"America/Barbados"/utf8>>, -14400},
                            {<<"America/Belem"/utf8>>, -10800},
                            {<<"America/Belize"/utf8>>, -21600},
                            {<<"America/Boa_Vista"/utf8>>, -14400},
                            {<<"America/Bogota"/utf8>>, -18000},
                            {<<"America/Boise"/utf8>>, -25200},
                            {<<"America/Cambridge_Bay"/utf8>>, -25200},
                            {<<"America/Campo_Grande"/utf8>>, -14400},
                            {<<"America/Cancun"/utf8>>, -18000},
                            {<<"America/Caracas"/utf8>>, -14400},
                            {<<"America/Cayenne"/utf8>>, -10800},
                            {<<"America/Chicago"/utf8>>, -21600},
                            {<<"America/Chihuahua"/utf8>>, -21600},
                            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
                            {<<"America/Costa_Rica"/utf8>>, -21600},
                            {<<"America/Cuiaba"/utf8>>, -14400},
                            {<<"America/Danmarkshavn"/utf8>>, 0},
                            {<<"America/Dawson"/utf8>>, -25200},
                            {<<"America/Dawson_Creek"/utf8>>, -25200},
                            {<<"America/Denver"/utf8>>, -25200},
                            {<<"America/Detroit"/utf8>>, -18000},
                            {<<"America/Edmonton"/utf8>>, -25200},
                            {<<"America/Eirunepe"/utf8>>, -18000},
                            {<<"America/El_Salvador"/utf8>>, -21600},
                            {<<"America/Fort_Nelson"/utf8>>, -25200},
                            {<<"America/Fortaleza"/utf8>>, -10800},
                            {<<"America/Glace_Bay"/utf8>>, -14400},
                            {<<"America/Goose_Bay"/utf8>>, -14400},
                            {<<"America/Grand_Turk"/utf8>>, -18000},
                            {<<"America/Guatemala"/utf8>>, -21600},
                            {<<"America/Guayaquil"/utf8>>, -18000},
                            {<<"America/Guyana"/utf8>>, -14400},
                            {<<"America/Halifax"/utf8>>, -14400},
                            {<<"America/Havana"/utf8>>, -18000},
                            {<<"America/Hermosillo"/utf8>>, -25200},
                            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
                            {<<"America/Indiana/Knox"/utf8>>, -21600},
                            {<<"America/Indiana/Marengo"/utf8>>, -18000},
                            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
                            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
                            {<<"America/Indiana/Vevay"/utf8>>, -18000},
                            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
                            {<<"America/Indiana/Winamac"/utf8>>, -18000},
                            {<<"America/Inuvik"/utf8>>, -25200},
                            {<<"America/Iqaluit"/utf8>>, -18000},
                            {<<"America/Jamaica"/utf8>>, -18000},
                            {<<"America/Juneau"/utf8>>, -32400},
                            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
                            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
                            {<<"America/La_Paz"/utf8>>, -14400},
                            {<<"America/Lima"/utf8>>, -18000},
                            {<<"America/Los_Angeles"/utf8>>, -28800},
                            {<<"America/Maceio"/utf8>>, -10800},
                            {<<"America/Managua"/utf8>>, -21600},
                            {<<"America/Manaus"/utf8>>, -14400},
                            {<<"America/Martinique"/utf8>>, -14400},
                            {<<"America/Matamoros"/utf8>>, -21600},
                            {<<"America/Mazatlan"/utf8>>, -25200},
                            {<<"America/Menominee"/utf8>>, -21600},
                            {<<"America/Merida"/utf8>>, -21600},
                            {<<"America/Metlakatla"/utf8>>, -32400},
                            {<<"America/Mexico_City"/utf8>>, -21600},
                            {<<"America/Miquelon"/utf8>>, -10800},
                            {<<"America/Moncton"/utf8>>, -14400},
                            {<<"America/Monterrey"/utf8>>, -21600},
                            {<<"America/Montevideo"/utf8>>, -10800},
                            {<<"America/New_York"/utf8>>, -18000},
                            {<<"America/Nome"/utf8>>, -32400},
                            {<<"America/Noronha"/utf8>>, -7200},
                            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
                            {<<"America/North_Dakota/Center"/utf8>>, -21600},
                            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
                            {<<"America/Nuuk"/utf8>>, -7200},
                            {<<"America/Ojinaga"/utf8>>, -21600},
                            {<<"America/Panama"/utf8>>, -18000},
                            {<<"America/Paramaribo"/utf8>>, -10800},
                            {<<"America/Phoenix"/utf8>>, -25200},
                            {<<"America/Port-au-Prince"/utf8>>, -18000},
                            {<<"America/Porto_Velho"/utf8>>, -14400},
                            {<<"America/Puerto_Rico"/utf8>>, -14400},
                            {<<"America/Punta_Arenas"/utf8>>, -10800},
                            {<<"America/Rankin_Inlet"/utf8>>, -21600},
                            {<<"America/Recife"/utf8>>, -10800},
                            {<<"America/Regina"/utf8>>, -21600},
                            {<<"America/Resolute"/utf8>>, -21600},
                            {<<"America/Rio_Branco"/utf8>>, -18000},
                            {<<"America/Santarem"/utf8>>, -10800},
                            {<<"America/Santiago"/utf8>>, -14400},
                            {<<"America/Santo_Domingo"/utf8>>, -14400},
                            {<<"America/Sao_Paulo"/utf8>>, -10800},
                            {<<"America/Scoresbysund"/utf8>>, -7200},
                            {<<"America/Sitka"/utf8>>, -32400},
                            {<<"America/St_Johns"/utf8>>, -12600},
                            {<<"America/Swift_Current"/utf8>>, -21600},
                            {<<"America/Tegucigalpa"/utf8>>, -21600},
                            {<<"America/Thule"/utf8>>, -14400},
                            {<<"America/Tijuana"/utf8>>, -28800},
                            {<<"America/Toronto"/utf8>>, -18000},
                            {<<"America/Vancouver"/utf8>>, -28800},
                            {<<"America/Whitehorse"/utf8>>, -25200},
                            {<<"America/Winnipeg"/utf8>>, -21600},
                            {<<"America/Yakutat"/utf8>>, -32400},
                            {<<"Antarctica/Casey"/utf8>>, 28800},
                            {<<"Antarctica/Davis"/utf8>>, 25200},
                            {<<"Antarctica/Macquarie"/utf8>>, 36000},
                            {<<"Antarctica/Mawson"/utf8>>, 18000},
                            {<<"Antarctica/Palmer"/utf8>>, -10800},
                            {<<"Antarctica/Rothera"/utf8>>, -10800},
                            {<<"Antarctica/Troll"/utf8>>, 0},
                            {<<"Antarctica/Vostok"/utf8>>, 18000},
                            {<<"Asia/Almaty"/utf8>>, 18000},
                            {<<"Asia/Amman"/utf8>>, 10800},
                            {<<"Asia/Anadyr"/utf8>>, 43200},
                            {<<"Asia/Aqtau"/utf8>>, 18000},
                            {<<"Asia/Aqtobe"/utf8>>, 18000},
                            {<<"Asia/Ashgabat"/utf8>>, 18000},
                            {<<"Asia/Atyrau"/utf8>>, 18000},
                            {<<"Asia/Baghdad"/utf8>>, 10800},
                            {<<"Asia/Baku"/utf8>>, 14400},
                            {<<"Asia/Bangkok"/utf8>>, 25200},
                            {<<"Asia/Barnaul"/utf8>>, 25200},
                            {<<"Asia/Beirut"/utf8>>, 7200},
                            {<<"Asia/Bishkek"/utf8>>, 21600},
                            {<<"Asia/Chita"/utf8>>, 32400},
                            {<<"Asia/Choibalsan"/utf8>>, 28800},
                            {<<"Asia/Colombo"/utf8>>, 19800},
                            {<<"Asia/Damascus"/utf8>>, 10800},
                            {<<"Asia/Dhaka"/utf8>>, 21600},
                            {<<"Asia/Dili"/utf8>>, 32400},
                            {<<"Asia/Dubai"/utf8>>, 14400},
                            {<<"Asia/Dushanbe"/utf8>>, 18000},
                            {<<"Asia/Famagusta"/utf8>>, 7200},
                            {<<"Asia/Gaza"/utf8>>, 7200},
                            {<<"Asia/Hebron"/utf8>>, 7200},
                            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
                            {<<"Asia/Hong_Kong"/utf8>>, 28800},
                            {<<"Asia/Hovd"/utf8>>, 25200},
                            {<<"Asia/Irkutsk"/utf8>>, 28800},
                            {<<"Asia/Jakarta"/utf8>>, 25200},
                            {<<"Asia/Jayapura"/utf8>>, 32400},
                            {<<"Asia/Jerusalem"/utf8>>, 7200},
                            {<<"Asia/Kabul"/utf8>>, 16200},
                            {<<"Asia/Kamchatka"/utf8>>, 43200},
                            {<<"Asia/Karachi"/utf8>>, 18000},
                            {<<"Asia/Kathmandu"/utf8>>, 20700},
                            {<<"Asia/Khandyga"/utf8>>, 32400},
                            {<<"Asia/Kolkata"/utf8>>, 19800},
                            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
                            {<<"Asia/Kuching"/utf8>>, 28800},
                            {<<"Asia/Macau"/utf8>>, 28800},
                            {<<"Asia/Magadan"/utf8>>, 39600},
                            {<<"Asia/Makassar"/utf8>>, 28800},
                            {<<"Asia/Manila"/utf8>>, 28800},
                            {<<"Asia/Nicosia"/utf8>>, 7200},
                            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
                            {<<"Asia/Novosibirsk"/utf8>>, 25200},
                            {<<"Asia/Omsk"/utf8>>, 21600},
                            {<<"Asia/Oral"/utf8>>, 18000},
                            {<<"Asia/Pontianak"/utf8>>, 25200},
                            {<<"Asia/Pyongyang"/utf8>>, 32400},
                            {<<"Asia/Qatar"/utf8>>, 10800},
                            {<<"Asia/Qostanay"/utf8>>, 18000},
                            {<<"Asia/Qyzylorda"/utf8>>, 18000},
                            {<<"Asia/Riyadh"/utf8>>, 10800},
                            {<<"Asia/Sakhalin"/utf8>>, 39600},
                            {<<"Asia/Samarkand"/utf8>>, 18000},
                            {<<"Asia/Seoul"/utf8>>, 32400},
                            {<<"Asia/Shanghai"/utf8>>, 28800},
                            {<<"Asia/Singapore"/utf8>>, 28800},
                            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
                            {<<"Asia/Taipei"/utf8>>, 28800},
                            {<<"Asia/Tashkent"/utf8>>, 18000},
                            {<<"Asia/Tbilisi"/utf8>>, 14400},
                            {<<"Asia/Tehran"/utf8>>, 12600},
                            {<<"Asia/Thimphu"/utf8>>, 21600},
                            {<<"Asia/Tokyo"/utf8>>, 32400},
                            {<<"Asia/Tomsk"/utf8>>, 25200},
                            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
                            {<<"Asia/Urumqi"/utf8>>, 21600},
                            {<<"Asia/Ust-Nera"/utf8>>, 36000},
                            {<<"Asia/Vladivostok"/utf8>>, 36000},
                            {<<"Asia/Yakutsk"/utf8>>, 32400},
                            {<<"Asia/Yangon"/utf8>>, 23400},
                            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
                            {<<"Asia/Yerevan"/utf8>>, 14400},
                            {<<"Atlantic/Azores"/utf8>>, -3600},
                            {<<"Atlantic/Bermuda"/utf8>>, -14400},
                            {<<"Atlantic/Canary"/utf8>>, 0},
                            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
                            {<<"Atlantic/Faroe"/utf8>>, 0},
                            {<<"Atlantic/Madeira"/utf8>>, 0},
                            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
                            {<<"Atlantic/Stanley"/utf8>>, -10800},
                            {<<"Australia/Adelaide"/utf8>>, 34200},
                            {<<"Australia/Brisbane"/utf8>>, 36000},
                            {<<"Australia/Broken_Hill"/utf8>>, 34200},
                            {<<"Australia/Darwin"/utf8>>, 34200},
                            {<<"Australia/Eucla"/utf8>>, 31500},
                            {<<"Australia/Hobart"/utf8>>, 36000},
                            {<<"Australia/Lindeman"/utf8>>, 36000},
                            {<<"Australia/Lord_Howe"/utf8>>, 37800},
                            {<<"Australia/Melbourne"/utf8>>, 36000},
                            {<<"Australia/Perth"/utf8>>, 28800},
                            {<<"Australia/Sydney"/utf8>>, 36000},
                            {<<"CET"/utf8>>, 3600},
                            {<<"CST6CDT"/utf8>>, -21600},
                            {<<"EET"/utf8>>, 7200},
                            {<<"EST"/utf8>>, -18000},
                            {<<"EST5EDT"/utf8>>, -18000},
                            {<<"Etc/GMT"/utf8>>, 0},
                            {<<"Etc/GMT+1"/utf8>>, -3600},
                            {<<"Etc/GMT+10"/utf8>>, -36000},
                            {<<"Etc/GMT+11"/utf8>>, -39600},
                            {<<"Etc/GMT+12"/utf8>>, -43200},
                            {<<"Etc/GMT+2"/utf8>>, -7200},
                            {<<"Etc/GMT+3"/utf8>>, -10800},
                            {<<"Etc/GMT+4"/utf8>>, -14400},
                            {<<"Etc/GMT+5"/utf8>>, -18000},
                            {<<"Etc/GMT+6"/utf8>>, -21600},
                            {<<"Etc/GMT+7"/utf8>>, -25200},
                            {<<"Etc/GMT+8"/utf8>>, -28800},
                            {<<"Etc/GMT+9"/utf8>>, -32400},
                            {<<"Etc/GMT-1"/utf8>>, 3600},
                            {<<"Etc/GMT-10"/utf8>>, 36000},
                            {<<"Etc/GMT-11"/utf8>>, 39600},
                            {<<"Etc/GMT-12"/utf8>>, 43200},
                            {<<"Etc/GMT-13"/utf8>>, 46800},
                            {<<"Etc/GMT-14"/utf8>>, 50400},
                            {<<"Etc/GMT-2"/utf8>>, 7200},
                            {<<"Etc/GMT-3"/utf8>>, 10800},
                            {<<"Etc/GMT-4"/utf8>>, 14400},
                            {<<"Etc/GMT-5"/utf8>>, 18000},
                            {<<"Etc/GMT-6"/utf8>>, 21600},
                            {<<"Etc/GMT-7"/utf8>>, 25200},
                            {<<"Etc/GMT-8"/utf8>>, 28800},
                            {<<"Etc/GMT-9"/utf8>>, 32400},
                            {<<"Etc/UTC"/utf8>>, 0},
                            {<<"Europe/Andorra"/utf8>>, 3600},
                            {<<"Europe/Astrakhan"/utf8>>, 14400},
                            {<<"Europe/Athens"/utf8>>, 7200},
                            {<<"Europe/Belgrade"/utf8>>, 3600},
                            {<<"Europe/Berlin"/utf8>>, 3600},
                            {<<"Europe/Brussels"/utf8>>, 3600},
                            {<<"Europe/Bucharest"/utf8>>, 7200},
                            {<<"Europe/Budapest"/utf8>>, 3600},
                            {<<"Europe/Chisinau"/utf8>>, 7200},
                            {<<"Europe/Dublin"/utf8>>, 3600},
                            {<<"Europe/Gibraltar"/utf8>>, 3600},
                            {<<"Europe/Helsinki"/utf8>>, 7200},
                            {<<"Europe/Istanbul"/utf8>>, 10800},
                            {<<"Europe/Kaliningrad"/utf8>>, 7200},
                            {<<"Europe/Kirov"/utf8>>, 10800},
                            {<<"Europe/Kyiv"/utf8>>, 7200},
                            {<<"Europe/Lisbon"/utf8>>, 0},
                            {<<"Europe/London"/utf8>>, 0},
                            {<<"Europe/Madrid"/utf8>>, 3600},
                            {<<"Europe/Malta"/utf8>>, 3600},
                            {<<"Europe/Minsk"/utf8>>, 10800},
                            {<<"Europe/Moscow"/utf8>>, 10800},
                            {<<"Europe/Paris"/utf8>>, 3600},
                            {<<"Europe/Prague"/utf8>>, 3600},
                            {<<"Europe/Riga"/utf8>>, 7200},
                            {<<"Europe/Rome"/utf8>>, 3600},
                            {<<"Europe/Samara"/utf8>>, 14400},
                            {<<"Europe/Saratov"/utf8>>, 14400},
                            {<<"Europe/Simferopol"/utf8>>, 10800},
                            {<<"Europe/Sofia"/utf8>>, 7200},
                            {<<"Europe/Tallinn"/utf8>>, 7200},
                            {<<"Europe/Tirane"/utf8>>, 3600},
                            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
                            {<<"Europe/Vienna"/utf8>>, 3600},
                            {<<"Europe/Vilnius"/utf8>>, 7200},
                            {<<"Europe/Volgograd"/utf8>>, 10800},
                            {<<"Europe/Warsaw"/utf8>>, 3600},
                            {<<"Europe/Zurich"/utf8>>, 3600},
                            {<<"HST"/utf8>>, -36000},
                            {<<"Indian/Chagos"/utf8>>, 21600},
                            {<<"Indian/Maldives"/utf8>>, 18000},
                            {<<"Indian/Mauritius"/utf8>>, 14400},
                            {<<"MET"/utf8>>, 3600},
                            {<<"MST"/utf8>>, -25200},
                            {<<"MST7MDT"/utf8>>, -25200},
                            {<<"PST8PDT"/utf8>>, -28800},
                            {<<"Pacific/Apia"/utf8>>, 46800},
                            {<<"Pacific/Auckland"/utf8>>, 43200},
                            {<<"Pacific/Bougainville"/utf8>>, 39600},
                            {<<"Pacific/Chatham"/utf8>>, 45900},
                            {<<"Pacific/Easter"/utf8>>, -21600},
                            {<<"Pacific/Efate"/utf8>>, 39600},
                            {<<"Pacific/Fakaofo"/utf8>>, 46800},
                            {<<"Pacific/Fiji"/utf8>>, 43200},
                            {<<"Pacific/Galapagos"/utf8>>, -21600},
                            {<<"Pacific/Gambier"/utf8>>, -32400},
                            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
                            {<<"Pacific/Guam"/utf8>>, 36000},
                            {<<"Pacific/Honolulu"/utf8>>, -36000},
                            {<<"Pacific/Kanton"/utf8>>, 46800},
                            {<<"Pacific/Kiritimati"/utf8>>, 50400},
                            {<<"Pacific/Kosrae"/utf8>>, 39600},
                            {<<"Pacific/Kwajalein"/utf8>>, 43200},
                            {<<"Pacific/Marquesas"/utf8>>, -34200},
                            {<<"Pacific/Nauru"/utf8>>, 43200},
                            {<<"Pacific/Niue"/utf8>>, -39600},
                            {<<"Pacific/Norfolk"/utf8>>, 39600},
                            {<<"Pacific/Noumea"/utf8>>, 39600},
                            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
                            {<<"Pacific/Palau"/utf8>>, 32400},
                            {<<"Pacific/Pitcairn"/utf8>>, -28800},
                            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
                            {<<"Pacific/Rarotonga"/utf8>>, -36000},
                            {<<"Pacific/Tahiti"/utf8>>, -36000},
                            {<<"Pacific/Tarawa"/utf8>>, 43200},
                            {<<"Pacific/Tongatapu"/utf8>>, 46800},
                            {<<"WET"/utf8>>, 0}],
                        fun(Item) -> erlang:element(1, Item) =:= Tz end
                    ) of
                        true ->
                            {some, Tz};

                        false ->
                            none
                    end
                end
            ),
            gleam@option:flatten(_pipe)
        end,
        {some, Monotonic_now}}.

-spec from_erlang_local_datetime(
    {{integer(), integer(), integer()}, {integer(), integer(), integer()}}
) -> time().
from_erlang_local_datetime(Erlang_datetime) ->
    {Date, Time} = Erlang_datetime,
    Offset_in_minutes = birl_ffi:local_offset(),
    _assert_subject = begin
        _pipe = {time, 0, 0, none, none},
        _pipe@1 = set_day(
            _pipe,
            {day,
                erlang:element(1, Date),
                erlang:element(2, Date),
                erlang:element(3, Date)}
        ),
        set_time_of_day(
            _pipe@1,
            {time_of_day,
                erlang:element(1, Time),
                erlang:element(2, Time),
                erlang:element(3, Time),
                0}
        )
    end,
    {time, Wall_time, _, none, none} = case _assert_subject of
        {time, _, _, none, none} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"from_erlang_local_datetime"/utf8>>,
                        line => 1268})
    end,
    Timezone = birl_ffi:local_timezone(),
    {time,
        Wall_time,
        Offset_in_minutes * 60000000,
        begin
            _pipe@2 = gleam@option:map(
                Timezone,
                fun(Tz) ->
                    case gleam@list:any(
                        [{<<"Africa/Abidjan"/utf8>>, 0},
                            {<<"Africa/Algiers"/utf8>>, 3600},
                            {<<"Africa/Bissau"/utf8>>, 0},
                            {<<"Africa/Cairo"/utf8>>, 7200},
                            {<<"Africa/Casablanca"/utf8>>, 3600},
                            {<<"Africa/Ceuta"/utf8>>, 3600},
                            {<<"Africa/El_Aaiun"/utf8>>, 3600},
                            {<<"Africa/Johannesburg"/utf8>>, 7200},
                            {<<"Africa/Juba"/utf8>>, 7200},
                            {<<"Africa/Khartoum"/utf8>>, 7200},
                            {<<"Africa/Lagos"/utf8>>, 3600},
                            {<<"Africa/Maputo"/utf8>>, 7200},
                            {<<"Africa/Monrovia"/utf8>>, 0},
                            {<<"Africa/Nairobi"/utf8>>, 10800},
                            {<<"Africa/Ndjamena"/utf8>>, 3600},
                            {<<"Africa/Sao_Tome"/utf8>>, 0},
                            {<<"Africa/Tripoli"/utf8>>, 7200},
                            {<<"Africa/Tunis"/utf8>>, 3600},
                            {<<"Africa/Windhoek"/utf8>>, 7200},
                            {<<"America/Adak"/utf8>>, -36000},
                            {<<"America/Anchorage"/utf8>>, -32400},
                            {<<"America/Araguaina"/utf8>>, -10800},
                            {<<"America/Argentina/Buenos_Aires"/utf8>>, -10800},
                            {<<"America/Argentina/Catamarca"/utf8>>, -10800},
                            {<<"America/Argentina/Cordoba"/utf8>>, -10800},
                            {<<"America/Argentina/Jujuy"/utf8>>, -10800},
                            {<<"America/Argentina/La_Rioja"/utf8>>, -10800},
                            {<<"America/Argentina/Mendoza"/utf8>>, -10800},
                            {<<"America/Argentina/Rio_Gallegos"/utf8>>, -10800},
                            {<<"America/Argentina/Salta"/utf8>>, -10800},
                            {<<"America/Argentina/San_Juan"/utf8>>, -10800},
                            {<<"America/Argentina/San_Luis"/utf8>>, -10800},
                            {<<"America/Argentina/Tucuman"/utf8>>, -10800},
                            {<<"America/Argentina/Ushuaia"/utf8>>, -10800},
                            {<<"America/Asuncion"/utf8>>, -14400},
                            {<<"America/Bahia"/utf8>>, -10800},
                            {<<"America/Bahia_Banderas"/utf8>>, -21600},
                            {<<"America/Barbados"/utf8>>, -14400},
                            {<<"America/Belem"/utf8>>, -10800},
                            {<<"America/Belize"/utf8>>, -21600},
                            {<<"America/Boa_Vista"/utf8>>, -14400},
                            {<<"America/Bogota"/utf8>>, -18000},
                            {<<"America/Boise"/utf8>>, -25200},
                            {<<"America/Cambridge_Bay"/utf8>>, -25200},
                            {<<"America/Campo_Grande"/utf8>>, -14400},
                            {<<"America/Cancun"/utf8>>, -18000},
                            {<<"America/Caracas"/utf8>>, -14400},
                            {<<"America/Cayenne"/utf8>>, -10800},
                            {<<"America/Chicago"/utf8>>, -21600},
                            {<<"America/Chihuahua"/utf8>>, -21600},
                            {<<"America/Ciudad_Juarez"/utf8>>, -25200},
                            {<<"America/Costa_Rica"/utf8>>, -21600},
                            {<<"America/Cuiaba"/utf8>>, -14400},
                            {<<"America/Danmarkshavn"/utf8>>, 0},
                            {<<"America/Dawson"/utf8>>, -25200},
                            {<<"America/Dawson_Creek"/utf8>>, -25200},
                            {<<"America/Denver"/utf8>>, -25200},
                            {<<"America/Detroit"/utf8>>, -18000},
                            {<<"America/Edmonton"/utf8>>, -25200},
                            {<<"America/Eirunepe"/utf8>>, -18000},
                            {<<"America/El_Salvador"/utf8>>, -21600},
                            {<<"America/Fort_Nelson"/utf8>>, -25200},
                            {<<"America/Fortaleza"/utf8>>, -10800},
                            {<<"America/Glace_Bay"/utf8>>, -14400},
                            {<<"America/Goose_Bay"/utf8>>, -14400},
                            {<<"America/Grand_Turk"/utf8>>, -18000},
                            {<<"America/Guatemala"/utf8>>, -21600},
                            {<<"America/Guayaquil"/utf8>>, -18000},
                            {<<"America/Guyana"/utf8>>, -14400},
                            {<<"America/Halifax"/utf8>>, -14400},
                            {<<"America/Havana"/utf8>>, -18000},
                            {<<"America/Hermosillo"/utf8>>, -25200},
                            {<<"America/Indiana/Indianapolis"/utf8>>, -18000},
                            {<<"America/Indiana/Knox"/utf8>>, -21600},
                            {<<"America/Indiana/Marengo"/utf8>>, -18000},
                            {<<"America/Indiana/Petersburg"/utf8>>, -18000},
                            {<<"America/Indiana/Tell_City"/utf8>>, -21600},
                            {<<"America/Indiana/Vevay"/utf8>>, -18000},
                            {<<"America/Indiana/Vincennes"/utf8>>, -18000},
                            {<<"America/Indiana/Winamac"/utf8>>, -18000},
                            {<<"America/Inuvik"/utf8>>, -25200},
                            {<<"America/Iqaluit"/utf8>>, -18000},
                            {<<"America/Jamaica"/utf8>>, -18000},
                            {<<"America/Juneau"/utf8>>, -32400},
                            {<<"America/Kentucky/Louisville"/utf8>>, -18000},
                            {<<"America/Kentucky/Monticello"/utf8>>, -18000},
                            {<<"America/La_Paz"/utf8>>, -14400},
                            {<<"America/Lima"/utf8>>, -18000},
                            {<<"America/Los_Angeles"/utf8>>, -28800},
                            {<<"America/Maceio"/utf8>>, -10800},
                            {<<"America/Managua"/utf8>>, -21600},
                            {<<"America/Manaus"/utf8>>, -14400},
                            {<<"America/Martinique"/utf8>>, -14400},
                            {<<"America/Matamoros"/utf8>>, -21600},
                            {<<"America/Mazatlan"/utf8>>, -25200},
                            {<<"America/Menominee"/utf8>>, -21600},
                            {<<"America/Merida"/utf8>>, -21600},
                            {<<"America/Metlakatla"/utf8>>, -32400},
                            {<<"America/Mexico_City"/utf8>>, -21600},
                            {<<"America/Miquelon"/utf8>>, -10800},
                            {<<"America/Moncton"/utf8>>, -14400},
                            {<<"America/Monterrey"/utf8>>, -21600},
                            {<<"America/Montevideo"/utf8>>, -10800},
                            {<<"America/New_York"/utf8>>, -18000},
                            {<<"America/Nome"/utf8>>, -32400},
                            {<<"America/Noronha"/utf8>>, -7200},
                            {<<"America/North_Dakota/Beulah"/utf8>>, -21600},
                            {<<"America/North_Dakota/Center"/utf8>>, -21600},
                            {<<"America/North_Dakota/New_Salem"/utf8>>, -21600},
                            {<<"America/Nuuk"/utf8>>, -7200},
                            {<<"America/Ojinaga"/utf8>>, -21600},
                            {<<"America/Panama"/utf8>>, -18000},
                            {<<"America/Paramaribo"/utf8>>, -10800},
                            {<<"America/Phoenix"/utf8>>, -25200},
                            {<<"America/Port-au-Prince"/utf8>>, -18000},
                            {<<"America/Porto_Velho"/utf8>>, -14400},
                            {<<"America/Puerto_Rico"/utf8>>, -14400},
                            {<<"America/Punta_Arenas"/utf8>>, -10800},
                            {<<"America/Rankin_Inlet"/utf8>>, -21600},
                            {<<"America/Recife"/utf8>>, -10800},
                            {<<"America/Regina"/utf8>>, -21600},
                            {<<"America/Resolute"/utf8>>, -21600},
                            {<<"America/Rio_Branco"/utf8>>, -18000},
                            {<<"America/Santarem"/utf8>>, -10800},
                            {<<"America/Santiago"/utf8>>, -14400},
                            {<<"America/Santo_Domingo"/utf8>>, -14400},
                            {<<"America/Sao_Paulo"/utf8>>, -10800},
                            {<<"America/Scoresbysund"/utf8>>, -7200},
                            {<<"America/Sitka"/utf8>>, -32400},
                            {<<"America/St_Johns"/utf8>>, -12600},
                            {<<"America/Swift_Current"/utf8>>, -21600},
                            {<<"America/Tegucigalpa"/utf8>>, -21600},
                            {<<"America/Thule"/utf8>>, -14400},
                            {<<"America/Tijuana"/utf8>>, -28800},
                            {<<"America/Toronto"/utf8>>, -18000},
                            {<<"America/Vancouver"/utf8>>, -28800},
                            {<<"America/Whitehorse"/utf8>>, -25200},
                            {<<"America/Winnipeg"/utf8>>, -21600},
                            {<<"America/Yakutat"/utf8>>, -32400},
                            {<<"Antarctica/Casey"/utf8>>, 28800},
                            {<<"Antarctica/Davis"/utf8>>, 25200},
                            {<<"Antarctica/Macquarie"/utf8>>, 36000},
                            {<<"Antarctica/Mawson"/utf8>>, 18000},
                            {<<"Antarctica/Palmer"/utf8>>, -10800},
                            {<<"Antarctica/Rothera"/utf8>>, -10800},
                            {<<"Antarctica/Troll"/utf8>>, 0},
                            {<<"Antarctica/Vostok"/utf8>>, 18000},
                            {<<"Asia/Almaty"/utf8>>, 18000},
                            {<<"Asia/Amman"/utf8>>, 10800},
                            {<<"Asia/Anadyr"/utf8>>, 43200},
                            {<<"Asia/Aqtau"/utf8>>, 18000},
                            {<<"Asia/Aqtobe"/utf8>>, 18000},
                            {<<"Asia/Ashgabat"/utf8>>, 18000},
                            {<<"Asia/Atyrau"/utf8>>, 18000},
                            {<<"Asia/Baghdad"/utf8>>, 10800},
                            {<<"Asia/Baku"/utf8>>, 14400},
                            {<<"Asia/Bangkok"/utf8>>, 25200},
                            {<<"Asia/Barnaul"/utf8>>, 25200},
                            {<<"Asia/Beirut"/utf8>>, 7200},
                            {<<"Asia/Bishkek"/utf8>>, 21600},
                            {<<"Asia/Chita"/utf8>>, 32400},
                            {<<"Asia/Choibalsan"/utf8>>, 28800},
                            {<<"Asia/Colombo"/utf8>>, 19800},
                            {<<"Asia/Damascus"/utf8>>, 10800},
                            {<<"Asia/Dhaka"/utf8>>, 21600},
                            {<<"Asia/Dili"/utf8>>, 32400},
                            {<<"Asia/Dubai"/utf8>>, 14400},
                            {<<"Asia/Dushanbe"/utf8>>, 18000},
                            {<<"Asia/Famagusta"/utf8>>, 7200},
                            {<<"Asia/Gaza"/utf8>>, 7200},
                            {<<"Asia/Hebron"/utf8>>, 7200},
                            {<<"Asia/Ho_Chi_Minh"/utf8>>, 25200},
                            {<<"Asia/Hong_Kong"/utf8>>, 28800},
                            {<<"Asia/Hovd"/utf8>>, 25200},
                            {<<"Asia/Irkutsk"/utf8>>, 28800},
                            {<<"Asia/Jakarta"/utf8>>, 25200},
                            {<<"Asia/Jayapura"/utf8>>, 32400},
                            {<<"Asia/Jerusalem"/utf8>>, 7200},
                            {<<"Asia/Kabul"/utf8>>, 16200},
                            {<<"Asia/Kamchatka"/utf8>>, 43200},
                            {<<"Asia/Karachi"/utf8>>, 18000},
                            {<<"Asia/Kathmandu"/utf8>>, 20700},
                            {<<"Asia/Khandyga"/utf8>>, 32400},
                            {<<"Asia/Kolkata"/utf8>>, 19800},
                            {<<"Asia/Krasnoyarsk"/utf8>>, 25200},
                            {<<"Asia/Kuching"/utf8>>, 28800},
                            {<<"Asia/Macau"/utf8>>, 28800},
                            {<<"Asia/Magadan"/utf8>>, 39600},
                            {<<"Asia/Makassar"/utf8>>, 28800},
                            {<<"Asia/Manila"/utf8>>, 28800},
                            {<<"Asia/Nicosia"/utf8>>, 7200},
                            {<<"Asia/Novokuznetsk"/utf8>>, 25200},
                            {<<"Asia/Novosibirsk"/utf8>>, 25200},
                            {<<"Asia/Omsk"/utf8>>, 21600},
                            {<<"Asia/Oral"/utf8>>, 18000},
                            {<<"Asia/Pontianak"/utf8>>, 25200},
                            {<<"Asia/Pyongyang"/utf8>>, 32400},
                            {<<"Asia/Qatar"/utf8>>, 10800},
                            {<<"Asia/Qostanay"/utf8>>, 18000},
                            {<<"Asia/Qyzylorda"/utf8>>, 18000},
                            {<<"Asia/Riyadh"/utf8>>, 10800},
                            {<<"Asia/Sakhalin"/utf8>>, 39600},
                            {<<"Asia/Samarkand"/utf8>>, 18000},
                            {<<"Asia/Seoul"/utf8>>, 32400},
                            {<<"Asia/Shanghai"/utf8>>, 28800},
                            {<<"Asia/Singapore"/utf8>>, 28800},
                            {<<"Asia/Srednekolymsk"/utf8>>, 39600},
                            {<<"Asia/Taipei"/utf8>>, 28800},
                            {<<"Asia/Tashkent"/utf8>>, 18000},
                            {<<"Asia/Tbilisi"/utf8>>, 14400},
                            {<<"Asia/Tehran"/utf8>>, 12600},
                            {<<"Asia/Thimphu"/utf8>>, 21600},
                            {<<"Asia/Tokyo"/utf8>>, 32400},
                            {<<"Asia/Tomsk"/utf8>>, 25200},
                            {<<"Asia/Ulaanbaatar"/utf8>>, 28800},
                            {<<"Asia/Urumqi"/utf8>>, 21600},
                            {<<"Asia/Ust-Nera"/utf8>>, 36000},
                            {<<"Asia/Vladivostok"/utf8>>, 36000},
                            {<<"Asia/Yakutsk"/utf8>>, 32400},
                            {<<"Asia/Yangon"/utf8>>, 23400},
                            {<<"Asia/Yekaterinburg"/utf8>>, 18000},
                            {<<"Asia/Yerevan"/utf8>>, 14400},
                            {<<"Atlantic/Azores"/utf8>>, -3600},
                            {<<"Atlantic/Bermuda"/utf8>>, -14400},
                            {<<"Atlantic/Canary"/utf8>>, 0},
                            {<<"Atlantic/Cape_Verde"/utf8>>, -3600},
                            {<<"Atlantic/Faroe"/utf8>>, 0},
                            {<<"Atlantic/Madeira"/utf8>>, 0},
                            {<<"Atlantic/South_Georgia"/utf8>>, -7200},
                            {<<"Atlantic/Stanley"/utf8>>, -10800},
                            {<<"Australia/Adelaide"/utf8>>, 34200},
                            {<<"Australia/Brisbane"/utf8>>, 36000},
                            {<<"Australia/Broken_Hill"/utf8>>, 34200},
                            {<<"Australia/Darwin"/utf8>>, 34200},
                            {<<"Australia/Eucla"/utf8>>, 31500},
                            {<<"Australia/Hobart"/utf8>>, 36000},
                            {<<"Australia/Lindeman"/utf8>>, 36000},
                            {<<"Australia/Lord_Howe"/utf8>>, 37800},
                            {<<"Australia/Melbourne"/utf8>>, 36000},
                            {<<"Australia/Perth"/utf8>>, 28800},
                            {<<"Australia/Sydney"/utf8>>, 36000},
                            {<<"CET"/utf8>>, 3600},
                            {<<"CST6CDT"/utf8>>, -21600},
                            {<<"EET"/utf8>>, 7200},
                            {<<"EST"/utf8>>, -18000},
                            {<<"EST5EDT"/utf8>>, -18000},
                            {<<"Etc/GMT"/utf8>>, 0},
                            {<<"Etc/GMT+1"/utf8>>, -3600},
                            {<<"Etc/GMT+10"/utf8>>, -36000},
                            {<<"Etc/GMT+11"/utf8>>, -39600},
                            {<<"Etc/GMT+12"/utf8>>, -43200},
                            {<<"Etc/GMT+2"/utf8>>, -7200},
                            {<<"Etc/GMT+3"/utf8>>, -10800},
                            {<<"Etc/GMT+4"/utf8>>, -14400},
                            {<<"Etc/GMT+5"/utf8>>, -18000},
                            {<<"Etc/GMT+6"/utf8>>, -21600},
                            {<<"Etc/GMT+7"/utf8>>, -25200},
                            {<<"Etc/GMT+8"/utf8>>, -28800},
                            {<<"Etc/GMT+9"/utf8>>, -32400},
                            {<<"Etc/GMT-1"/utf8>>, 3600},
                            {<<"Etc/GMT-10"/utf8>>, 36000},
                            {<<"Etc/GMT-11"/utf8>>, 39600},
                            {<<"Etc/GMT-12"/utf8>>, 43200},
                            {<<"Etc/GMT-13"/utf8>>, 46800},
                            {<<"Etc/GMT-14"/utf8>>, 50400},
                            {<<"Etc/GMT-2"/utf8>>, 7200},
                            {<<"Etc/GMT-3"/utf8>>, 10800},
                            {<<"Etc/GMT-4"/utf8>>, 14400},
                            {<<"Etc/GMT-5"/utf8>>, 18000},
                            {<<"Etc/GMT-6"/utf8>>, 21600},
                            {<<"Etc/GMT-7"/utf8>>, 25200},
                            {<<"Etc/GMT-8"/utf8>>, 28800},
                            {<<"Etc/GMT-9"/utf8>>, 32400},
                            {<<"Etc/UTC"/utf8>>, 0},
                            {<<"Europe/Andorra"/utf8>>, 3600},
                            {<<"Europe/Astrakhan"/utf8>>, 14400},
                            {<<"Europe/Athens"/utf8>>, 7200},
                            {<<"Europe/Belgrade"/utf8>>, 3600},
                            {<<"Europe/Berlin"/utf8>>, 3600},
                            {<<"Europe/Brussels"/utf8>>, 3600},
                            {<<"Europe/Bucharest"/utf8>>, 7200},
                            {<<"Europe/Budapest"/utf8>>, 3600},
                            {<<"Europe/Chisinau"/utf8>>, 7200},
                            {<<"Europe/Dublin"/utf8>>, 3600},
                            {<<"Europe/Gibraltar"/utf8>>, 3600},
                            {<<"Europe/Helsinki"/utf8>>, 7200},
                            {<<"Europe/Istanbul"/utf8>>, 10800},
                            {<<"Europe/Kaliningrad"/utf8>>, 7200},
                            {<<"Europe/Kirov"/utf8>>, 10800},
                            {<<"Europe/Kyiv"/utf8>>, 7200},
                            {<<"Europe/Lisbon"/utf8>>, 0},
                            {<<"Europe/London"/utf8>>, 0},
                            {<<"Europe/Madrid"/utf8>>, 3600},
                            {<<"Europe/Malta"/utf8>>, 3600},
                            {<<"Europe/Minsk"/utf8>>, 10800},
                            {<<"Europe/Moscow"/utf8>>, 10800},
                            {<<"Europe/Paris"/utf8>>, 3600},
                            {<<"Europe/Prague"/utf8>>, 3600},
                            {<<"Europe/Riga"/utf8>>, 7200},
                            {<<"Europe/Rome"/utf8>>, 3600},
                            {<<"Europe/Samara"/utf8>>, 14400},
                            {<<"Europe/Saratov"/utf8>>, 14400},
                            {<<"Europe/Simferopol"/utf8>>, 10800},
                            {<<"Europe/Sofia"/utf8>>, 7200},
                            {<<"Europe/Tallinn"/utf8>>, 7200},
                            {<<"Europe/Tirane"/utf8>>, 3600},
                            {<<"Europe/Ulyanovsk"/utf8>>, 14400},
                            {<<"Europe/Vienna"/utf8>>, 3600},
                            {<<"Europe/Vilnius"/utf8>>, 7200},
                            {<<"Europe/Volgograd"/utf8>>, 10800},
                            {<<"Europe/Warsaw"/utf8>>, 3600},
                            {<<"Europe/Zurich"/utf8>>, 3600},
                            {<<"HST"/utf8>>, -36000},
                            {<<"Indian/Chagos"/utf8>>, 21600},
                            {<<"Indian/Maldives"/utf8>>, 18000},
                            {<<"Indian/Mauritius"/utf8>>, 14400},
                            {<<"MET"/utf8>>, 3600},
                            {<<"MST"/utf8>>, -25200},
                            {<<"MST7MDT"/utf8>>, -25200},
                            {<<"PST8PDT"/utf8>>, -28800},
                            {<<"Pacific/Apia"/utf8>>, 46800},
                            {<<"Pacific/Auckland"/utf8>>, 43200},
                            {<<"Pacific/Bougainville"/utf8>>, 39600},
                            {<<"Pacific/Chatham"/utf8>>, 45900},
                            {<<"Pacific/Easter"/utf8>>, -21600},
                            {<<"Pacific/Efate"/utf8>>, 39600},
                            {<<"Pacific/Fakaofo"/utf8>>, 46800},
                            {<<"Pacific/Fiji"/utf8>>, 43200},
                            {<<"Pacific/Galapagos"/utf8>>, -21600},
                            {<<"Pacific/Gambier"/utf8>>, -32400},
                            {<<"Pacific/Guadalcanal"/utf8>>, 39600},
                            {<<"Pacific/Guam"/utf8>>, 36000},
                            {<<"Pacific/Honolulu"/utf8>>, -36000},
                            {<<"Pacific/Kanton"/utf8>>, 46800},
                            {<<"Pacific/Kiritimati"/utf8>>, 50400},
                            {<<"Pacific/Kosrae"/utf8>>, 39600},
                            {<<"Pacific/Kwajalein"/utf8>>, 43200},
                            {<<"Pacific/Marquesas"/utf8>>, -34200},
                            {<<"Pacific/Nauru"/utf8>>, 43200},
                            {<<"Pacific/Niue"/utf8>>, -39600},
                            {<<"Pacific/Norfolk"/utf8>>, 39600},
                            {<<"Pacific/Noumea"/utf8>>, 39600},
                            {<<"Pacific/Pago_Pago"/utf8>>, -39600},
                            {<<"Pacific/Palau"/utf8>>, 32400},
                            {<<"Pacific/Pitcairn"/utf8>>, -28800},
                            {<<"Pacific/Port_Moresby"/utf8>>, 36000},
                            {<<"Pacific/Rarotonga"/utf8>>, -36000},
                            {<<"Pacific/Tahiti"/utf8>>, -36000},
                            {<<"Pacific/Tarawa"/utf8>>, 43200},
                            {<<"Pacific/Tongatapu"/utf8>>, 46800},
                            {<<"WET"/utf8>>, 0}],
                        fun(Item) -> erlang:element(1, Item) =:= Tz end
                    ) of
                        true ->
                            {some, Tz};

                        false ->
                            none
                    end
                end
            ),
            gleam@option:flatten(_pipe@2)
        end,
        none}.

-spec from_erlang_universal_datetime(
    {{integer(), integer(), integer()}, {integer(), integer(), integer()}}
) -> time().
from_erlang_universal_datetime(Erlang_datetime) ->
    {Date, Time} = Erlang_datetime,
    _assert_subject = begin
        _pipe = {time, 0, 0, none, none},
        _pipe@1 = set_day(
            _pipe,
            {day,
                erlang:element(1, Date),
                erlang:element(2, Date),
                erlang:element(3, Date)}
        ),
        _pipe@2 = set_time_of_day(
            _pipe@1,
            {time_of_day,
                erlang:element(1, Time),
                erlang:element(2, Time),
                erlang:element(3, Time),
                0}
        ),
        set_timezone(_pipe@2, <<"Etc/UTC"/utf8>>)
    end,
    {ok, New_value} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl"/utf8>>,
                        function => <<"from_erlang_universal_datetime"/utf8>>,
                        line => 1295})
    end,
    New_value.

-spec parse_relative(time(), binary()) -> {ok, time()} | {error, nil}.
parse_relative(Origin, Legible_difference) ->
    case gleam@string:split(Legible_difference, <<" "/utf8>>) of
        [<<"in"/utf8>>, Amount_string, Unit] ->
            Unit@1 = case gleam@string:ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_right(Unit, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"from now"/utf8>>] ->
            Unit@1 = case gleam@string:ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_right(Unit, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"later"/utf8>>] ->
            Unit@1 = case gleam@string:ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_right(Unit, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"ahead"/utf8>>] ->
            Unit@1 = case gleam@string:ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_right(Unit, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"in the future"/utf8>>] ->
            Unit@1 = case gleam@string:ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_right(Unit, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string, Unit, <<"hence"/utf8>>] ->
            Unit@1 = case gleam@string:ends_with(Unit, <<"s"/utf8>>) of
                false ->
                    Unit;

                true ->
                    gleam@string:drop_right(Unit, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string),
                fun(Amount) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@1
                        ),
                        fun(Unit@2) ->
                            {ok,
                                add(
                                    Origin,
                                    birl@duration:new([{Amount, Unit@2}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"ago"/utf8>>] ->
            Unit@4 = case gleam@string:ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_right(Unit@3, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"before"/utf8>>] ->
            Unit@4 = case gleam@string:ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_right(Unit@3, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"earlier"/utf8>>] ->
            Unit@4 = case gleam@string:ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_right(Unit@3, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"since"/utf8>>] ->
            Unit@4 = case gleam@string:ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_right(Unit@3, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        [Amount_string@1, Unit@3, <<"in the past"/utf8>>] ->
            Unit@4 = case gleam@string:ends_with(Unit@3, <<"s"/utf8>>) of
                false ->
                    Unit@3;

                true ->
                    gleam@string:drop_right(Unit@3, 1)
            end,
            gleam@result:then(
                gleam@int:parse(Amount_string@1),
                fun(Amount@1) ->
                    gleam@result:then(
                        gleam@list:key_find(
                            [{<<"year"/utf8>>, year},
                                {<<"month"/utf8>>, month},
                                {<<"week"/utf8>>, week},
                                {<<"day"/utf8>>, day},
                                {<<"hour"/utf8>>, hour},
                                {<<"minute"/utf8>>, minute},
                                {<<"second"/utf8>>, second}],
                            Unit@4
                        ),
                        fun(Unit@5) ->
                            {ok,
                                subtract(
                                    Origin,
                                    birl@duration:new([{Amount@1, Unit@5}])
                                )}
                        end
                    )
                end
            );

        _ ->
            {error, nil}
    end.

-spec legible_difference(time(), time()) -> binary().
legible_difference(A, B) ->
    case begin
        _pipe = difference(A, B),
        birl@duration:blur(_pipe)
    end of
        {_, micro_second} ->
            <<"just now"/utf8>>;

        {_, milli_second} ->
            <<"just now"/utf8>>;

        {Amount, Unit} ->
            _assert_subject = gleam@list:key_find(
                [{year, <<"year"/utf8>>},
                    {month, <<"month"/utf8>>},
                    {week, <<"week"/utf8>>},
                    {day, <<"day"/utf8>>},
                    {hour, <<"hour"/utf8>>},
                    {minute, <<"minute"/utf8>>},
                    {second, <<"second"/utf8>>}],
                Unit
            ),
            {ok, Unit@1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"birl"/utf8>>,
                                function => <<"legible_difference"/utf8>>,
                                line => 963})
            end,
            Is_negative = Amount < 0,
            Amount@1 = gleam@int:absolute_value(Amount),
            Unit@2 = case Amount@1 of
                1 ->
                    Unit@1;

                _ ->
                    <<Unit@1/binary, "s"/utf8>>
            end,
            case Is_negative of
                true ->
                    <<<<<<"in "/utf8, (gleam@int:to_string(Amount@1))/binary>>/binary,
                            " "/utf8>>/binary,
                        Unit@2/binary>>;

                false ->
                    <<<<<<(begin
                                    _pipe@1 = Amount@1,
                                    _pipe@2 = gleam@int:absolute_value(_pipe@1),
                                    gleam@int:to_string(_pipe@2)
                                end)/binary,
                                " "/utf8>>/binary,
                            Unit@2/binary>>/binary,
                        " ago"/utf8>>
            end
    end.

-spec parse_weekday(binary()) -> {ok, weekday()} | {error, nil}.
parse_weekday(Value) ->
    Lowercase = gleam@string:lowercase(Value),
    Weekday = gleam@list:find(
        [{mon, {<<"Monday"/utf8>>, <<"Mon"/utf8>>}},
            {tue, {<<"Tuesday"/utf8>>, <<"Tue"/utf8>>}},
            {wed, {<<"Wednesday"/utf8>>, <<"Wed"/utf8>>}},
            {thu, {<<"Thursday"/utf8>>, <<"Thu"/utf8>>}},
            {fri, {<<"Friday"/utf8>>, <<"Fri"/utf8>>}},
            {sat, {<<"Saturday"/utf8>>, <<"Sat"/utf8>>}},
            {sun, {<<"Sunday"/utf8>>, <<"Sun"/utf8>>}}],
        fun(Weekday_string) ->
            {_, {Long, Short}} = Weekday_string,
            (Lowercase =:= gleam@string:lowercase(Short)) orelse (Lowercase =:= gleam@string:lowercase(
                Long
            ))
        end
    ),
    _pipe = Weekday,
    gleam@result:map(_pipe, fun(Weekday@1) -> erlang:element(1, Weekday@1) end).

-spec from_http(binary()) -> {ok, time()} | {error, nil}.
from_http(Value) ->
    Value@1 = gleam@string:trim(Value),
    gleam@result:then(
        gleam@string:split_once(Value@1, <<","/utf8>>),
        fun(_use0) ->
            {Weekday, Rest} = _use0,
            gleam@bool:guard(
                not gleam@list:any(
                    [{mon, {<<"Monday"/utf8>>, <<"Mon"/utf8>>}},
                        {tue, {<<"Tuesday"/utf8>>, <<"Tue"/utf8>>}},
                        {wed, {<<"Wednesday"/utf8>>, <<"Wed"/utf8>>}},
                        {thu, {<<"Thursday"/utf8>>, <<"Thu"/utf8>>}},
                        {fri, {<<"Friday"/utf8>>, <<"Fri"/utf8>>}},
                        {sat, {<<"Saturday"/utf8>>, <<"Sat"/utf8>>}},
                        {sun, {<<"Sunday"/utf8>>, <<"Sun"/utf8>>}}],
                    fun(Weekday_item) ->
                        Strings = erlang:element(2, Weekday_item),
                        (erlang:element(1, Strings) =:= Weekday) orelse (erlang:element(
                            2,
                            Strings
                        )
                        =:= Weekday)
                    end
                ),
                {error, nil},
                fun() ->
                    Rest@1 = gleam@string:trim(Rest),
                    _assert_subject = gleam@regex:from_string(<<"\\s+"/utf8>>),
                    {ok, Whitespace_pattern} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail,
                                        module => <<"birl"/utf8>>,
                                        function => <<"from_http"/utf8>>,
                                        line => 737})
                    end,
                    case gleam@regex:split(Whitespace_pattern, Rest@1) of
                        [Day_string,
                            Month_string,
                            Year_string,
                            Time_string,
                            Offset_string] ->
                            Time_string@1 = gleam@string:replace(
                                Time_string,
                                <<":"/utf8>>,
                                <<""/utf8>>
                            ),
                            case {gleam@int:parse(Day_string),
                                begin
                                    _pipe = gleam@list:index_map(
                                        [{jan,
                                                {<<"January"/utf8>>,
                                                    <<"Jan"/utf8>>}},
                                            {feb,
                                                {<<"February"/utf8>>,
                                                    <<"Feb"/utf8>>}},
                                            {mar,
                                                {<<"March"/utf8>>,
                                                    <<"Mar"/utf8>>}},
                                            {apr,
                                                {<<"April"/utf8>>,
                                                    <<"Apr"/utf8>>}},
                                            {may,
                                                {<<"May"/utf8>>, <<"May"/utf8>>}},
                                            {jun,
                                                {<<"June"/utf8>>,
                                                    <<"Jun"/utf8>>}},
                                            {jul,
                                                {<<"July"/utf8>>,
                                                    <<"Jul"/utf8>>}},
                                            {aug,
                                                {<<"August"/utf8>>,
                                                    <<"Aug"/utf8>>}},
                                            {sep,
                                                {<<"September"/utf8>>,
                                                    <<"Sep"/utf8>>}},
                                            {oct,
                                                {<<"October"/utf8>>,
                                                    <<"Oct"/utf8>>}},
                                            {nov,
                                                {<<"November"/utf8>>,
                                                    <<"Nov"/utf8>>}},
                                            {dec,
                                                {<<"December"/utf8>>,
                                                    <<"Dec"/utf8>>}}],
                                        fun(Month, Index) ->
                                            Strings@1 = erlang:element(2, Month),
                                            {Index,
                                                erlang:element(1, Strings@1),
                                                erlang:element(2, Strings@1)}
                                        end
                                    ),
                                    gleam@list:find(
                                        _pipe,
                                        fun(Month@1) ->
                                            (erlang:element(2, Month@1) =:= Month_string)
                                            orelse (erlang:element(3, Month@1)
                                            =:= Month_string)
                                        end
                                    )
                                end,
                                gleam@int:parse(Year_string),
                                parse_time_section(Time_string@1)} of
                                {{ok, Day},
                                    {ok, {Month_index, _, _}},
                                    {ok, Year},
                                    {ok, [Hour, Minute, Second]}} ->
                                    case from_parts(
                                        {Year, Month_index + 1, Day},
                                        {Hour, Minute, Second, 0},
                                        case Offset_string of
                                            <<"GMT"/utf8>> ->
                                                <<"Z"/utf8>>;

                                            _ ->
                                                Offset_string
                                        end
                                    ) of
                                        {ok, Value@2} ->
                                            Correct_weekday = string_weekday(
                                                Value@2
                                            ),
                                            Correct_short_weekday = short_string_weekday(
                                                Value@2
                                            ),
                                            case gleam@list:contains(
                                                [Correct_weekday,
                                                    Correct_short_weekday],
                                                Weekday
                                            ) of
                                                true ->
                                                    {ok, Value@2};

                                                false ->
                                                    {error, nil}
                                            end;

                                        {error, nil} ->
                                            {error, nil}
                                    end;

                                {_, _, _, _} ->
                                    {error, nil}
                            end;

                        [Day_string@1, Time_string@2, Offset_string@1] ->
                            case gleam@string:split(Day_string@1, <<"-"/utf8>>) of
                                [Day_string@2, Month_string@1, Year_string@1] ->
                                    Time_string@3 = gleam@string:replace(
                                        Time_string@2,
                                        <<":"/utf8>>,
                                        <<""/utf8>>
                                    ),
                                    case {gleam@int:parse(Day_string@2),
                                        begin
                                            _pipe@1 = gleam@list:index_map(
                                                [{jan,
                                                        {<<"January"/utf8>>,
                                                            <<"Jan"/utf8>>}},
                                                    {feb,
                                                        {<<"February"/utf8>>,
                                                            <<"Feb"/utf8>>}},
                                                    {mar,
                                                        {<<"March"/utf8>>,
                                                            <<"Mar"/utf8>>}},
                                                    {apr,
                                                        {<<"April"/utf8>>,
                                                            <<"Apr"/utf8>>}},
                                                    {may,
                                                        {<<"May"/utf8>>,
                                                            <<"May"/utf8>>}},
                                                    {jun,
                                                        {<<"June"/utf8>>,
                                                            <<"Jun"/utf8>>}},
                                                    {jul,
                                                        {<<"July"/utf8>>,
                                                            <<"Jul"/utf8>>}},
                                                    {aug,
                                                        {<<"August"/utf8>>,
                                                            <<"Aug"/utf8>>}},
                                                    {sep,
                                                        {<<"September"/utf8>>,
                                                            <<"Sep"/utf8>>}},
                                                    {oct,
                                                        {<<"October"/utf8>>,
                                                            <<"Oct"/utf8>>}},
                                                    {nov,
                                                        {<<"November"/utf8>>,
                                                            <<"Nov"/utf8>>}},
                                                    {dec,
                                                        {<<"December"/utf8>>,
                                                            <<"Dec"/utf8>>}}],
                                                fun(Month@2, Index@1) ->
                                                    Strings@2 = erlang:element(
                                                        2,
                                                        Month@2
                                                    ),
                                                    {Index@1,
                                                        erlang:element(
                                                            1,
                                                            Strings@2
                                                        ),
                                                        erlang:element(
                                                            2,
                                                            Strings@2
                                                        )}
                                                end
                                            ),
                                            gleam@list:find(
                                                _pipe@1,
                                                fun(Month@3) ->
                                                    (erlang:element(2, Month@3)
                                                    =:= Month_string@1)
                                                    orelse (erlang:element(
                                                        3,
                                                        Month@3
                                                    )
                                                    =:= Month_string@1)
                                                end
                                            )
                                        end,
                                        gleam@int:parse(Year_string@1),
                                        parse_time_section(Time_string@3)} of
                                        {{ok, Day@1},
                                            {ok, {Month_index@1, _, _}},
                                            {ok, Year@1},
                                            {ok, [Hour@1, Minute@1, Second@1]}} ->
                                            case from_parts(
                                                {Year@1,
                                                    Month_index@1 + 1,
                                                    Day@1},
                                                {Hour@1, Minute@1, Second@1, 0},
                                                case Offset_string@1 of
                                                    <<"GMT"/utf8>> ->
                                                        <<"Z"/utf8>>;

                                                    _ ->
                                                        Offset_string@1
                                                end
                                            ) of
                                                {ok, Value@3} ->
                                                    Correct_weekday@1 = string_weekday(
                                                        Value@3
                                                    ),
                                                    Correct_short_weekday@1 = short_string_weekday(
                                                        Value@3
                                                    ),
                                                    case gleam@list:contains(
                                                        [Correct_weekday@1,
                                                            Correct_short_weekday@1],
                                                        Weekday
                                                    ) of
                                                        true ->
                                                            {ok, Value@3};

                                                        false ->
                                                            {error, nil}
                                                    end;

                                                {error, nil} ->
                                                    {error, nil}
                                            end;

                                        {_, _, _, _} ->
                                            {error, nil}
                                    end;

                                _ ->
                                    {error, nil}
                            end;

                        _ ->
                            {error, nil}
                    end
                end
            )
        end
    ).

-spec parse_month(binary()) -> {ok, month()} | {error, nil}.
parse_month(Value) ->
    Lowercase = gleam@string:lowercase(Value),
    Month = gleam@list:find(
        [{jan, {<<"January"/utf8>>, <<"Jan"/utf8>>}},
            {feb, {<<"February"/utf8>>, <<"Feb"/utf8>>}},
            {mar, {<<"March"/utf8>>, <<"Mar"/utf8>>}},
            {apr, {<<"April"/utf8>>, <<"Apr"/utf8>>}},
            {may, {<<"May"/utf8>>, <<"May"/utf8>>}},
            {jun, {<<"June"/utf8>>, <<"Jun"/utf8>>}},
            {jul, {<<"July"/utf8>>, <<"Jul"/utf8>>}},
            {aug, {<<"August"/utf8>>, <<"Aug"/utf8>>}},
            {sep, {<<"September"/utf8>>, <<"Sep"/utf8>>}},
            {oct, {<<"October"/utf8>>, <<"Oct"/utf8>>}},
            {nov, {<<"November"/utf8>>, <<"Nov"/utf8>>}},
            {dec, {<<"December"/utf8>>, <<"Dec"/utf8>>}}],
        fun(Month_string) ->
            {_, {Long, Short}} = Month_string,
            (Lowercase =:= gleam@string:lowercase(Short)) orelse (Lowercase =:= gleam@string:lowercase(
                Long
            ))
        end
    ),
    _pipe = Month,
    gleam@result:map(_pipe, fun(Month@1) -> erlang:element(1, Month@1) end).
