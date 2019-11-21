-module(erlwx).

-export([
		 get_access_token/2,
		 check_session_key/3,
		 code_2_session/3,
		 check_signature/3,
		 check_wx_biz_data_crypt/4
		]).

-export([
		 test_get_access_token/0,
		 test_check_session_key/0,
		 test_code_2_session/0
		]).

get_access_token(AppId, Secret) ->
	erlwx_api:get_access_token(AppId, Secret).

check_session_key(AccessToken, Signature, OpenId) ->
	erlwx_api:check_session_key(AccessToken, Signature, OpenId).

code_2_session(AppId, Secret, Js_Code) ->
	erlwx_api:code_2_session(AppId, Secret, Js_Code).

check_signature(RawData, Signature1, SessionKey) ->
	Signature2 = erlwx_util:sha1(<<RawData/binary, SessionKey/binary>>),
	Signature1 == Signature2.

check_wx_biz_data_crypt(AppId, SessionKey, Iv, EncryptedData) ->
	case wx_biz_data_crypt:decrypt(AppId, SessionKey, Iv, EncryptedData) of
		{ok, DencryptedData} ->
			{ok, DencryptedData};
		{error, Reason} ->
			{error, Reason}
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

