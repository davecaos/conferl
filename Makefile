PROJECT = conferl

DEPS = lager cowboy  shotgun sumo_db inaka_mixer barrel_jiffy xref_runner uuid

TEST_DEPS = shotgun katana_test

SHELL_DEPS = sync

dep_lager        = hex 3.0.2
dep_cowboy       = git git://github.com/ninenines/cowboy.git  2.0.0-pre.3
dep_inaka_mixer  = hex 0.1.5
dep_barrel_jiffy = hex 0.14.4
dep_shotgun      = hex 0.2.3
dep_xref_runner  = hex 0.2.4
dep_sync         = git https://github.com/rustyio/sync.git     9c78e7b
dep_sumo_db      = git https://github.com/inaka/sumo_db.git    0.4.0
dep_uuid         = git git://github.com/okeuday/uuid.git       v1.5.1
dep_katana_test = git https://github.com/inaka/katana-test.git 0.0.5

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

test-build-shell: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-shell:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

test-shell: test-build-shell
	$(gen_verbose) erl $(SHELL_PATH) -pa test $(TEST_SHELL_OPTS)

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-shell plt

dialyze-all: app test-build-shell dialyze