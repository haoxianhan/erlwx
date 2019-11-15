-module(erlwx_util).

-export([
		 http_get/1,
		 http_post/2,
		 hmac_sha256/2
		]).

-export([
		 test_hmac_sha256/0
		]).

http_get(Url) ->
	Method = get,
	Request = {Url, []},
	case httpc:request(Method, Request, [{timeout, 3000}, {connect_timeout, 1000}], []) of
		{ok, {_StatusLine, _Headers, Body}} ->
			{ok, Body};
		{error, Reason} ->
			{error, Reason};
		_ ->
			{error, unknow}
	end.


http_post(Url, Body) ->
	Method = post,
	Request = {Url, [], "application/json", Body},
	case httpc:request(Method, Request, [{timeout, 3000}, {connect_timeout, 1000}], []) of
		{ok, {_StatusLine, _Headers, Body}} ->
			{ok, Body};
		{error, Reason} ->
			{error, Reason};
		_ ->
			{error, unknow}
	end.

hmac_sha256(Key, Data) ->
	<<Mac:256/big-unsigned-integer>> = crypto:hmac(sha256, Key, Data),
	lists:flatten(io_lib:format("~64.16.0b", [Mac])).



test_hmac_sha256() ->
	Key = "o0q0otL8aEzpcZL/FT9WsQ==",
	Data = "{\"foo\":\"bar\"}",
	hmac_sha256(Key, Data). %% 654571f79995b2ce1e149e53c0a33dc39c0a74090db514261454e8dbe432aa0b
