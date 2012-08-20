
TEST_SUPPORT = \
	test/etap.beam \
	test/test_util.beam

%.beam: %.erl
	erlc -o test/ $<

all: compile

compile:
	echo "==> Build emonk & dependencies"
	./rebar compile

verbose:
	./rebar compile verbose=1

check: all $(TEST_SUPPORT)
	prove test/*.t

clean:
	./rebar clean
	rm -f test/*.beam
