PROJECT = emqx_retainer
PROJECT_DESCRIPTION = EMQ X Retainer
PROJECT_VERSION = 3.0

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqx/cuttlefish

TEST_DEPS = emqx_ct_helpers emqx_management
dep_emqx_ct_helpers = git https://github.com/emqx/emqx-ct-helpers
dep_emqx_management = git https://github.com/emqx/emqx-management emqx30

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +warnings_as_errors +warn_export_all +warn_unused_import

TEST_ERLC_OPTS += +debug_info

EUNIT_OPTS = verbose

CT_SUITES = emqx_retainer

CT_OPTS = -cover test/ct.cover.spec -erl_args -name emqxct@127.0.0.1

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_retainer.conf -i priv/emqx_retainer.schema -d data

