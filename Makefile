PROJECT = conferl

DEPS = lager cowboy sumo_db mixer jiffy xref_runner uuid goldrush
BUILD_DEPS = inaka_mk hexer_mk
TEST_DEPS = shotgun katana_test

SHELL_DEPS = sync

dep_lager        = hex 3.0.2
dep_cowboy       = git https://github.com/ninenines/cowboy.git  2.0.0-pre.3
dep_mixer        = git https://github.com/inaka/mixer.git       0.1.4
dep_jiffy        = git https://github.com/davisp/jiffy.git      0.14.8
dep_shotgun      = hex 0.2.3
dep_xref_runner  = hex 0.2.4
dep_sync         = git https://github.com/rustyio/sync.git      9c78e7b
dep_sumo_db      = git https://github.com/inaka/sumo_db.git     0.4.0
dep_uuid         = git https://github.com/okeuday/uuid.git      v1.5.1
dep_katana_test  = git https://github.com/inaka/katana-test.git 0.0.5
dep_goldrush     = git https://github.com/DeadZen/goldrush      0.1.8
dep_inaka_mk     = git https://github.com/inaka/inaka.mk.git    1.0.0
dep_hexer_mk     = git https://github.com/inaka/hexer.mk.git    1.1.0

DEP_PLUGINS = inaka_mk hexer_mk

PLT_APPS := inets
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Wrace_conditions

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_OPTS = -erl_args -config rel/sys.config

SHELL_OPTS = -name ${PROJECT}@`hostname` -s sync -s ${PROJECT} -config rel/sys.config

testshell:
	erl -pa ebin -pa deps/*/ebin -pa test -config rel/sys.config -s sync
