compile:
	./rebar compile

clean_generate:
	rm -rf rel/widgy

generate:
	cd rel && ../rebar generate

console:
	./rel/widgy/bin/widgy console

quick-console:
	erl -config rel/files/sys.config -pa apps/*/ebin -pa deps/*/ebin -boot start_sasl -eval 'application:start(cowboy),application:start(widgy)'

start_nginx:
	nginx -p . -c ./nginx.conf

all: compile clean_generate generate
