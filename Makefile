PROJECT = emqx_retainer
PROJECT_DESCRIPTION = EMQ X Retainer
PROJECT_VERSION = 3.0

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx emqx30
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.0

TEST_DEPS = emqx_management
dep_emqx_management = git-emqx https://github.com/emqx/emqx-management emqx30

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +warnings_as_errors +warn_export_all +warn_unused_import

TEST_ERLC_OPTS += +debug_info

EUNIT_OPTS = verbose

CT_SUITES = emqx_retainer

CT_OPTS = -cover test/ct.cover.spec -erl_args -name emqxct@127.0.0.1

COVER = true

define dep_fetch_git-emqx
	git clone -q --depth 1 -b $(call dep_commit,$(1)) -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)) > /dev/null 2>&1; \
	cd $(DEPS_DIR)/$(call dep_name,$(1));
endef

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_retainer.conf -i priv/emqx_retainer.schema -d data

