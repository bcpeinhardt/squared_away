{application, mist, [
    {mod, {'mist@internal@clock', []}},
    {vsn, "2.0.0"},
    {applications, [birl,
                    gleam_erlang,
                    gleam_http,
                    gleam_otp,
                    gleam_stdlib,
                    glisten,
                    gramps,
                    hpack,
                    logging]},
    {description, "a misty Gleam web server"},
    {modules, []},
    {registered, []}
]}.
