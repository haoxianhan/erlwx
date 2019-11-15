-module(erlwx).

-export([
		 get_access_token/2,
		 check_session_key/3,
		 code_2_session/3
		]).

-export([
		 test_get_access_token/0,
		 % test_check_session_key/0,
		 test_code_2_session/0
		]).

get_access_token(AppId, Secret) ->
	get_access_token("client_credential", AppId, Secret).
get_access_token(Grant_Type, AppId, Secret) ->
	Url = lists:concat(["https://api.weixin.qq.com/cgi-bin/token?",
						"grant_type=", Grant_Type,
						"&appid=", AppId,
						"&secret=", Secret]),
	Res = case erlwx_util:http_get(Url) of
		{ok, Body} ->
			case jsx:decode(list_to_binary(Body), [return_maps]) of
				#{<<"access_token">> := AccessToken,
				  <<"expires_in">> := ExpiresIn} ->
					{ok, AccessToken, ExpiresIn};
				#{<<"errcode">> := ErrCode,
				  <<"errmsg">> := ErrMsg} ->
					{error, ErrCode, ErrMsg};
				_ ->
					{error, json_to_data_error, Body}
			end;
		_ ->
			{error, http_error, Url}
	end,
	io:format("get_access_token ~p~n", [Res]),
	Res.


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
			io:format("check_session_key ~p~n", [Body]);
		_ ->
			{error, connect_error}
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
			io:format("code_2_session ~p~n", [Body]);
		_ ->
			{error, connect_error}
	end.



-define(APP_ID, "wx7cdbc414c579f5df").
-define(APP_SECRET, "a4c046f33b4921f2066cefc06d0f5363").
test_get_access_token() ->
	spawn_link(?MODULE, get_access_token, [?APP_ID,  "a8c046f33b4921f2066cefc06d0f53635"]),
	spawn_link(?MODULE, get_access_token, [?APP_ID, ?APP_SECRET]).

% test_check_session_key() ->
%     check_session_key(?APP_ID, ?APP_SECRET).


test_code_2_session() ->
	code_2_session(?APP_ID, ?APP_SECRET, "12346588234").
