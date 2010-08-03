emonk - Accidentally Refactored erlang_js
=========================================

[erlang_js][erlang_js] is awesome. But I started refactoring. Now there's emonk.

With the latest versions of Emonk there's quite a bit of difference now. Emonk
is NIF based and uses a thread-pool to move JavaScript execution off of the
Erlang scheduler threads. Translation from Erlang terms to JavaScript objects
uses a native translation step to avoid JSON overhead. I haven't gotten around
to actually testing to see if there's any sort of appreciable difference in
speed.

[erlang_js]: http://hg.basho.com/erlang_js/

Requisites
----------

Emonk will require a bit more in terms of infrastructure because the build
system doesn't carry a copy of its own Spidermonkey code. I did this because I
felt quite bad about untaring and building a package as part of the build
system. Most of the emonk's build system is stolen directly from Noah Slater's
work on CouchDB so if something is broken, blame me and fill out a ticket.

Other than that, the basic requirements:

1. Fairly recent version of Spidermonkey. See the SPIDERMONKEY file for more
   info.
2. R14A. Uses the new NIF API extensively.

Building
--------

Hopefully the build scenario is something like:

    $ git clone git://github.com/davisp/emonk.git
    $ cd emonk
    $ make
    $ make check

Running
-------

I've been using [etap][etap] to test as I code. Its awesome. You should use it.
That said, running is pretty cake assuming emonk is on your Erlang code path:

    $ ERL_LIBS=~/awesome_projects/emonk/src erl -s emonk
    1> {ok, Context} = emonk:new_context().
    {ok, <<>>} % Note: The <<>> here is *not* an empty binary. Its a resource.
    2> emonk:eval(Context, <<"var f = 2; f*3;">>).
    {ok, 6}
    3> emonk:eval(Context, <<"var g = function(x) {return x*4;};">>).
    {ok, undefined}
    4> emonk:call(Context, <<"g">>, [9]).
    {ok, 36}

[etap]: http://github.com/ngerakines/etap

Bugs and Things
---------------

The best place to file [bugs][bugs] is at the [issue][bugs] tracker on
[GitHub][github]. Less yapping, more tapping. Thanks [mattly][mattly].

[bugs]: http://github.com/davisp/emonk/issues
[github]: http://github.com/davisp
[mattly]: http://twitter.com/mattly

Yep
---

That's pretty much it. Mostly this was internal churn that I kinda started
chasing around like my tail. Hopefully things are easy enough to follow in the
logs and code.
