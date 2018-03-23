PROJECT = emq_retainer
PROJECT_DESCRIPTION = EMQ Retainer
PROJECT_VERSION = 2.3.6

DEPS = clique

dep_clique  = git https://github.com/emqtt/clique

BUILD_DEPS = emqttd cuttlefish
dep_emqttd     = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_retainer.conf -i priv/emq_retainer.schema -d data
