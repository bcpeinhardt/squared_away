-record(cli, {
    run :: fun((lustre_dev_tools@cli:env()) -> {lustre_dev_tools@cli:env(),
        {ok, any()} | {error, lustre_dev_tools@error:error()}})
}).
