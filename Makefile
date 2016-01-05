PROJECT = conferl

DEPS = lager cowboy  sumo_db inaka_mixer barrel_jiffy xref_runner uuid

TEST_DEPS = shotgun

SHELL_DEPS = sync

dep_lager        = hex 3.0.2
dep_sync         = git https://github.com/rustyio/sync.git    9c78e7b
dep_sumo_db      = git https://github.com/inaka/sumo_db.git   0.3.13
dep_cowboy       = hex 1.0.4
dep_inaka_mixer  = hex 0.1.5
dep_barrel_jiffy = hex 0.14.4
dep_shotgun     = git git://github.com/inaka/shotgun.git     0.1.12
dep_xref_runner = hex 0.2.4
dep_uuid        = git git://github.com/okeuday/uuid.git      v1.5.1

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

quicktests: app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
