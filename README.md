Web UI Bro
==========

An Erlang server and JavaScript library that allow you to
live-broadcast UI events on a webpage.

NOT YET: Also supports non-live playback.

Copyright 2012 Andrew Shu

Mouse pointer image from
http://www.nbdtech.com/Blog/archive/2009/05/21/free-mouse-pointer-image.aspx

Running it: Ubuntu
------------------

    $ cp nginx/erlang-web-ui-broadcast /etc/nginx/sites-available/
    $ ln -s /etc/nginx/sites-{available,enabled}/erlang-web-ui-broadcast
    $ unlink /etc/nginx/sites-enabled/default
    $ /etc/init.d/nginx restart

    $ rebar get-deps clean compile
    $ ./start.sh

Then goto http://localhost/demo.html using multiple browsers on different IPs.

