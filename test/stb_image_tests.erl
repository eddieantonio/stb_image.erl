-module(stb_image_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertException(error, badarg, stb_image:load(foo)).

load_test() ->
    {ok, Image} = stb_image:load("/Users/eddieantonio/good.jpg"),
    ?assert(is_binary(Image)).
