-module(wx_biz_data_crypt).

-include_lib("eunit/include/eunit.hrl").

-export([
         decrypt/4,

         test/0,
		 test1/0
        ]).


decrypt(AppId, RawSessionKey, RawIv, RawEncryptedData) ->
    SessionKey = base64:mime_decode(RawSessionKey),
    Iv = base64:mime_decode(RawIv),
    EncryptedData = base64:mime_decode(RawEncryptedData),
    DecryptData = crypto:block_decrypt(aes_cbc128 , SessionKey, Iv, EncryptedData),
    FormJson = jsx:decode(pkcs7:unpad(DecryptData), [return_maps]),
    case check_water_mark(AppId, FormJson) of
        {ok} ->
            {ok, FormJson};
        {error, Reason} ->
            {error, Reason}
    end.

check_water_mark(AppId, FormJson) ->
    case FormJson of
        #{<<"watermark">> := #{<<"appid">> := AppIdX,
                               <<"timestamp">> := _TimeStamp} } ->
            case binary_to_list(AppIdX) == AppId of
                true ->
                    {ok};
                _ ->
                    {error, error_invalid}
            end;
        _ ->
            {error, error_invalid}
    end.


test() ->
    AppId = "wx4f4bc4dec97d474b",
    RawSessionKey = "tiihtNczf5v6AKRyjwEUhQ==",
    RawEncryptedData = "CiyLU1Aw2KjvrjMdj8YKliAjtP4gsMZMQmRzooG2xrDcvSnxIMXFufNstNGTyaGS9uT5geRa0W4oTOb1WT7fJlAC+oNPdbB+3hVbJSRgv+4lGOETKUQz6OYStslQ142dNCuabNPGBzlooOmB231qMM85d2/fV6ChevvXvQP8Hkue1poOFtnEtpyxVLW1zAo6/1Xx1COxFvrc2d7UL/lmHInNlxuacJXwu0fjpXfz/YqYzBIBzD6WUfTIF9GRHpOn/Hz7saL8xz+W//FRAUid1OksQaQx4CMs8LOddcQhULW4ucetDf96JcR3g0gfRK4PC7E/r7Z6xNrXd2UIeorGj5Ef7b1pJAYB6Y5anaHqZ9J6nKEBvB4DnNLIVWSgARns/8wR2SiRS7MNACwTyrGvt9ts8p12PKFdlqYTopNHR1Vf7XjfhQlVsAJdNiKdYmYVoKlaRv85IfVunYzO0IKXsyl7JCUjCpoG20f0a04COwfneQAGGwd5oa+T8yO5hzuyDb/XcxxmK01EpqOyuxINew==",
    RawIv = "r7BXXKkLb8qrSNn05n0qiA==",
    Res1 = decrypt(AppId, RawSessionKey, RawIv, RawEncryptedData),
    Res2 = {ok, #{<<"avatarUrl">> =>
                  <<"http://wx.qlogo.cn/mmopen/vi_32/aSKcBBPpibyKNicHNTMM0qJVh8Kjgiak2AHWr8MHM4WgMEm7GFhsf8OYrySdbvAMvTsw3mo8ibKicsnfN5pRjl1p8HQ/0">>,
                  <<"city">> => <<"Guangzhou">>,<<"country">> => <<"CN">>,
                  <<"gender">> => 1,<<"language">> => <<"zh_CN">>,
                  <<"nickName">> => <<"Band">>,
                  <<"openId">> => <<"oGZUI0egBJY1zhBYw2KhdUfwVJJE">>,
                  <<"province">> => <<"Guangdong">>,
                  <<"unionId">> => <<"ocMvos6NjeKLIBqg5Mr9QjxrP1FA">>,
                  <<"watermark">> =>
                  #{<<"appid">> => <<"wx4f4bc4dec97d474b">>,
                    <<"timestamp">> => 1477314187}}},
    ?assertEqual(Res1, Res2).

test1() ->
    AppId = "wx7cdbc414c579f5df",
    RawSessionKey = "IbnrOHcXBX74cW21mTRTeA==",
    RawEncryptedData = "TUgeRfdL8xI6kbMjKR6NI20BH0L5FCGmdWNvl4MY1TwFRcvorGNyfD0LsjiPmLgDbD+jr08uI0WnVudN8rRotlMFFUZK4QihOk9/uH1TJdMhh6rilq3TxirzU8H5WAU71OwZ8mfcjQ8hoifar6ZHIwfH2HwD/mJPkZ01qNut8PFga5qZ1rGYtgR2dy5QohzpXh0kvMC9R8sTJ7dvG/6VVbNbbKjsPZQfkEy0E6Al3Ke6neb7wbB9m7zfh0QL2nQJOYxMLaa7XNhheAtioCnfM+3SO2B0NfDxVd7oB09bregj9kDeXqb9G2qKFP5ae3UkMaIvdY2bSCA58p6SjcHzLW7bmZTVnueaGt0pbg7YGE9EJqjTfsFWa6c7FoUKKw31xtxwwTLOqSBcB3iW6px2Vk4yS7b3DWlfe59vMsV4AQ597GJAQfrSRA6q0gCpuGFG4bQCkmiXmKQHOmz0rsTvIsz5g4DIssYhvXsER+yBVbU=",
    RawIv = "uMNj7ViKRkvX9S8uenMlzA==",
    decrypt(AppId, RawSessionKey, RawIv, RawEncryptedData).
