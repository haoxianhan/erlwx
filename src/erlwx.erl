-module(erlwx).

-include("error.hrl").

-export([
		 get_access_token/2,
		 check_session_key/3,
		 code_2_session/3,
		 check_signature/3,
		 check_wx_biz_data_crypt/4
		]).

get_access_token(AppId, Secret) ->
	erlwx_api:get_access_token(AppId, Secret).

check_session_key(AccessToken, Signature, OpenId) ->
	erlwx_api:check_session_key(AccessToken, Signature, OpenId).

code_2_session(AppId, Secret, Js_Code) ->
	erlwx_api:code_2_session(AppId, Secret, Js_Code).

check_signature(RawData, Signature1, SessionKey) ->
	case erlwx_util:sha1(RawData ++ SessionKey) of
		Signature2 when Signature1 == Signature2 ->
			{ok};
		_ ->
			{error, ?ERLWX_ERROR_CHECK_SIGN, check_sign_error}
	end.

check_wx_biz_data_crypt(AppId, SessionKey, Iv, EncryptedData) ->
	case wx_biz_data_crypt:decrypt(AppId, SessionKey, Iv, EncryptedData) of
		{ok, DencryptedData} ->
			{ok, DencryptedData};
		{error, Reason} ->
			{error, ?ERLWX_ERROR_BIZ_DATA_CRYPT, Reason}
	end.

