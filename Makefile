all:
	rebar compile

test: all
	rebar eunit
