PROJECT = jtrack

DEPS = cowboy jiffy hackney uuid sync
dep_cowboy_commit = 2.13.0

dep_uuid = git https://github.com/okeuday/uuid
dep_jiffy = git https://github.com/davisp/jiffy
dep_hackney = git https://github.com/benoitc/hackney
dep_sync = git https://github.com/rustyio/sync

REL_DEPS += relx

DEP_PLUGINS = cowboy

include erlang.mk
