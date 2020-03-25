-module(erlwx_api).

-export([
		 get_access_token/2,
		 check_session_key/3,
		 code_2_session/3
		]).

-export([
		 test_get_access_token/0,
		 test_check_session_key/0,
		 test_code_2_session/0
		]).

-define(ERLWX_ERROR_JSON_TO_DATA,  99901).
-define(ERLWX_ERROR_HTTP_ERROR,    99902).

get_access_token(AppId, Secret) ->
	get_access_token("client_credential", AppId, Secret).
get_access_token(Grant_Type, AppId, Secret) ->
	Url = lists:concat(["https://api.weixin.qq.com/cgi-bin/token?",
						"grant_type=", Grant_Type,
						"&appid=", AppId,
						"&secret=", Secret]),
	case erlwx_util:http_get(Url) of
		{ok, Body} ->
			case jsx:decode(list_to_binary(Body), [return_maps]) of
				#{<<"access_token">> := AccessToken,
				  <<"expires_in">> := ExpiresIn} ->
					{ok, AccessToken, ExpiresIn};
				#{<<"errcode">> := ErrCode,
				  <<"errmsg">> := ErrMsg} ->
					{error, ErrCode, ErrMsg};
				_ ->
					{error, ?ERLWX_ERROR_JSON_TO_DATA, Body}
			end;
		_ ->
			{error, ?ERLWX_ERROR_HTTP_ERROR, Url}
	end.


check_session_key(AccessToken, Signature, OpenId) ->
	check_session_key(AccessToken, Signature, OpenId, "hmac_sha256").
check_session_key(AccessToken, Signature, OpenId, Sig_Method) ->
	Url = lists:concat(["https://api.weixin.qq.com/wxa/checksession?",
						"access_token=", AccessToken,
						"&signature=", Signature,
						"&openid=", OpenId,
						"&sig_method=", Sig_Method]),
	case erlwx_util:http_get(Url) of
		{ok, Body} ->
			case jsx:decode(list_to_binary(Body), [return_maps]) of
				#{<<"errcode">> := ErrCode} when ErrCode == 0->
					{ok};
				#{<<"errcode">> := ErrCode,
				  <<"errmsg">> := ErrMsg} ->
					{error, ErrCode, ErrMsg};
				_ ->
					{error, ?ERLWX_ERROR_JSON_TO_DATA, Body}
			end;
		_ ->
			{error, ?ERLWX_ERROR_HTTP_ERROR, Url}
	end.

code_2_session(AppId, Secret, Js_Code) ->
	code_2_session(AppId, Secret, Js_Code, "authorization_code").
code_2_session(AppId, Secret, Js_Code, Grant_Type) ->
	Url = lists:concat(["https://api.weixin.qq.com/sns/jscode2session?",
						"appid=", AppId,
						"&secret=", Secret,
						"&js_code=", Js_Code,
						"&grant_type=", Grant_Type]),
	case erlwx_util:http_get(Url) of
		{ok, Body} ->
			case jsx:decode(list_to_binary(Body), [return_maps]) of
				#{<<"openid">> := OpenId,
				  <<"session_key">> := SessionKey,
				  <<"unionid">> := UnionId} ->
					{ok, OpenId, SessionKey, UnionId};
				#{<<"openid">> := OpenId,
				  <<"session_key">> := SessionKey} ->
					{ok, OpenId, SessionKey, <<"0">>};
				#{<<"errcode">> := ErrCode,
				  <<"errmsg">> := ErrMsg} ->
					{error, ErrCode, ErrMsg};
				_ ->
					{error, ?ERLWX_ERROR_JSON_TO_DATA, Body}
			end;
		_ ->
			{error, ?ERLWX_ERROR_HTTP_ERROR, Url}
	end.


-define(APP_ID, "wx7cdbc414c579f5df").
-define(APP_SECRET, "a4c046f33b4921f2066cefc06d0f5363").
test_get_access_token() ->
	spawn(fun() -> io:format("~p~n", [get_access_token(?APP_ID,  "a8c046f33b4921f2066cefc06d0f53635")]) end),
	spawn(fun() -> io:format("~p~n", [get_access_token(?APP_ID, ?APP_SECRET)]) end).

test_check_session_key() ->
	check_session_key("", "", "").

test_code_2_session() ->
	spawn(fun() -> io:format("~p~n", [code_2_session(?APP_ID, ?APP_SECRET, "043j2Z8r0aBHhm1Gkb7r0gyV8r0j2Z81")]) end).
