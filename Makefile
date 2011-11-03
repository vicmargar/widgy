compile:
	./rebar compile

clean_generate:
	rm -rf rel/widgy

generate:
	cd rel && ../rebar generate

console:
	./rel/widgy/bin/widgy console

quick-console:
	erl -config rel/files/sys.config -pa  apps/*/ebin -pa vendor/*/ebin -pa -boot start_sasl -eval "reloader:start(),application:start(widgy)"

all: compile clean_generate generate