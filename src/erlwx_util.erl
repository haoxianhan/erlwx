-module(erlwx_util).

-export([
		 http_get/1,
		 http_post/2,

		 sha1/1,
		 hmac_sha256/2,

		 hexstring/1
		]).

-export([
		 test_sha1/0,
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

sha1(Data) ->
	Hash = crypto:hash(sha, Data),
	hexstring(Hash).

hmac_sha256(Key, Data) ->
	HMAC = crypto:hmac(sha256, Key, Data),
	hexstring(HMAC).

hexstring(<<X:128/big-unsigned-integer>>) ->
	lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
	lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
	lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
	lists:flatten(io_lib:format("~128.16.0b", [X])).



test_sha1() ->
	Data = "{\"nickName\":\"Band\",\"gender\":1,\"language\":\"zh_CN\",\"city\":\"Guangzhou\",\"province\":\"Guangdong\",\"country\":\"CN\",\"avatarUrl\":\"http://wx.qlogo.cn/mmopen/vi_32/1vZvI39NWFQ9XM4LtQpFrQJ1xlgZxx3w7bQxKARol6503Iuswjjn6nIGBiaycAjAtpujxyzYsrztuuICqIM5ibXQ/0\"}HyVFkGl5F5OQWJZZaNzBBg==",
	sha1(Data) == "75e81ceda165f4ffa64f4068af58c64b8f54b88c".

test_hmac_sha256() ->
	Key = "o0q0otL8aEzpcZL/FT9WsQ==",
	Data = "{\"foo\":\"bar\"}",
	hmac_sha256(Key, Data) == "654571f79995b2ce1e149e53c0a33dc39c0a74090db514261454e8dbe432aa0b".

