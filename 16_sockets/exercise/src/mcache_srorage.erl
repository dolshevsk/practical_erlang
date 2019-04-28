-module(mcache_storage).

-record(state, {storage = maps:new()}).