-module(stb_image).
-export([load/1, load/2]).
-on_load(init/0).

-type stb_image() :: {
        X :: non_neg_integer(),
        Y :: non_neg_integer(),
        NComp :: components(),
        Data :: binary()
       }.
-type req_comp() :: req_comp_atom() | components().
-type req_comp_atom() :: default | grey | grey_alpha | rgb | rgb_alpha.
-type components() :: 0 | 1 | 2 | 3 | 4.
-type load_options() :: req_comp() | [{depth, req_comp()}].
-type load_error_reason() :: atom() | {stb_image, string()}.
-type load_return() :: {ok, stb_image()} |
                       {error, Reason :: load_error_reason()}.

% The name of the application we're writing. This is the name
% used for the Erlang .app file.

-define(APPNAME, stb_image).

% The name of the shared library we're going to load the NIF
% code from. Defined in rebar.config as so_name.

-define(LIBNAME, stb_image).

%% API

% NIF functions end up overriding the functions defined in this module. But
% this module must define the functions we want the NIF to implement.
% Theoretically this won't ever get called as out on_load function init/0
% should raise an error if we have issues.
%
% A really nice person would make a pure Erlang fallback incase a NIF was
% unable to load for a specific platform.

load(Filename) -> load(Filename, []).

-spec load(Filename :: iolist(), Options :: load_options()) -> load_return().
load(Filename, Depth) when is_integer(Depth) or is_atom(Depth) ->
    load(Filename, [{depth, Depth}]);
load(Filename, Options) ->
    DesiredDepth = proplists:get_value(depth, Options, default),
    % NOTE: coerces any filename into zero-terminated UTF-8 buffer.
    FileNameBin = unicode:characters_to_binary([Filename, 0]),
    internal_load(FileNameBin, req_comp(DesiredDepth)).

%% Internal functions

% Since we used init/0 in our -on_load() preprocessor directive, this
% function will get called as the module is loaded. This is the perfect
% place to load up our NIF shared library. Handily, the response of
% erlang:load_nif/2 matches the return specification for -on_load()
% functions.

-spec internal_load(Filename :: binary(), Depth :: components()) ->
    load_return().
internal_load(_Filename, _DesiredDepth) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

% https://github.com/nothings/stb/blob/master/stb_image.h#L394
-spec req_comp(req_comp()) -> components().
req_comp(X) when is_integer(X),
                 X >= 0, X =< 4 -> X;
req_comp(default)     -> 0;
req_comp(grey)        -> 1;
req_comp(grey_alpha)  -> 2;
req_comp(rgb)         -> 3;
req_comp(rgb_alpha)   -> 4;
req_comp(_) -> exit(badarg).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).
