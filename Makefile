PROJECT = emq_mod_retainer
PROJECT_DESCRIPTION = Retainer Module
PROJECT_VERSION = 2.0.5
BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_mod_retainer.conf -i priv/emq_mod_retainer.schema -d data

