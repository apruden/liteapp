#!/bin/sh
erl -pa ebin deps/*/ebin -s liteapp_app +K true \
	-eval "io:format(\"Point your browser at http://localhost:8999~n\")."
