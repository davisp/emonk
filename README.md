emonk - Accidentally Refactored erlang_js
=========================================

[erlang_js][erlangjs] is awesome. But I started refactoring. Now there's emonk.

[erlangjs]: http://hg.basho.com/erlang_js/

Requisites
----------

Emonk will require a bit more in terms of infrastructure because the build system doesn't carry a copy of its own Spidermonkey code. I did this because I felt quite bad about untaring and building a package as part of the build system. Most of the emonk's build system is stolen directly from Noah Slater's work on CouchDB so if something is broken, blame me and fill out a ticket.

Other than that, the basic requirements:

1. Fairly recent version of Spidermonkey. See the SPIDERMONKEY file for more info.
2. Somewhat recent Erlang. Not sure because I haven't tested.

Building
--------

Hopefully the build scenario is something like:

    $ git clone git://github.com/davisp/emonk.git
    $ cd emonk
    $ ./bootstrap
    $ ./configure
    $ make
    $ make check
    $ make install # Not tested, been using make check.

Running
-------

I've been using [etap][etap] to test as I code. Its awesome. You should use it. That said, running is pretty cake assuming emonk is on your Erlang code path:

    $ ERL_LIBS=~/awesome_projects/emonk/src erl -s emonk
    1> {ok, Port} = emonk:new().
    {ok, Port<stuff>}
    2> emonk:eval(Port, <<"var f = 2; f*3;">>).
    {ok, 6}
    3> emonk:eval(Port, <<"var g = function(x) {return x*4;};">>).
    {ok, undefined}
    4> emonk:call(Port, <<"g">>, [9]).
    {ok, 36}
    5> emonk:destroy(Port).
    ok

[etap]: http://github.com/ngerakines/etap

Yep
---

That's pretty much it. Mostly this was internal churn that I kinda started chasing around like my tail. Hopefully things are easy enough to follow in the logs and code. I'm skimping a bit on the docs here because its late and I want to watch The Big Bang Theory before bed.