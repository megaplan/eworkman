-ifndef(eworkman_params).
-define(eworkman_params, true).

-define(T, 1000).
-define(TC, 0).
-define(LOG, "/var/log/erpher/ewm").
-define(CONF, "eworkman.conf").

% state of a worker gen_server
-record(child, {
    name,
    port,
    id,
    os_pid,
    debug
}).

-record(chi, {
    pid,
    id,
    mon,
    os_pid,
    start={0,0,0} % time in now() format
}).

-record(pool, {
    id,
    w_duration = 86400, % seconds
    worker_config,
    workers = [] :: [#chi{}],
    waiting = [], % waiting for restart
    restart_delay,
    restart_policy, % restart, none, delay
    min_workers = 5
}).

% state of a workers handler
-record(ewm, {
    w_pools = [],
    web_server_pid,
    web_server_opts,
    log,
    debug
}).

-endif.
