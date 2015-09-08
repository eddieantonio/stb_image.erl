-module(stb_image_tests).

-include_lib("eunit/include/eunit.hrl").

% Test loading any regular image.
load_test() ->
    Filename = filename_for(any_image),
    ?assertMatch({ok, _Image}, stb_image:load(Filename)).

% Test loading any regular image.
load_info_test() ->
    Filename = filename_for(xterm_table),
    Result = stb_image:load(Filename),
    ?assertMatch({ok, {_, _, _, Data}} when is_binary(Data), Result),
    {ok, {X, Y, NComp, Data}} = Result,
    ?assertEqual(X, 12),
    ?assertEqual(Y, 22),
    ?assertEqual(NComp, 3),
    Size = X * Y * NComp,
    % Find the 6th pixel, and return its value.
    % The 6th pixel happens to be green!
    ?assertMatch(<<_Pixels:(5*24), 0, 16#ff, 0, _/binary>>, Data),
    ?assertMatch(<<_:Size/unit:8>>, Data).

% Test loading any regular image.
load_req_comp_test() ->
    Filename = filename_for(xterm_table),
    Result = stb_image:load(Filename, grey),
    ?assertMatch({ok, {_, _, _, Data}} when is_binary(Data), Result),
    {ok, {X, Y, NComp, Data}} = Result,
    ?assertEqual(NComp, 1),
    % Test the first 3 pixel values, one byte each.
    ?assertMatch(<<16#0, 16#37, 16#4f, _/binary>>, Data),
    Size = X * Y * NComp,
    ?assertMatch(<<_:Size/unit:8>>, Data),
    % Load the image again, using a prop list.
    Result2 = stb_image:load(Filename, [{depth, 1}]),
    {ok, {_, _, _, OtherData}} = Result2,
    ?assertMatch(Data, OtherData).

% Test loading any regular image.
load_byte_size_test() ->
    {X, Y} = {12, 22},
    Filename = filename_for(xterm_table),
    {ok, {X, Y, 1, Grey}} = stb_image:load(Filename, [{depth, grey}]),
    {ok, {X, Y, 2, GreyAlpha}} = stb_image:load(Filename, [{depth, grey_alpha}]),
    {ok, {X, Y, 3, RGB}} = stb_image:load(Filename, [{depth, rgb}]),
    {ok, {X, Y, 4, RGBA}} = stb_image:load(Filename, [{depth, rgb_alpha}]),

    % Ensure the binary sizes make sense.
    [SizeGrey, SizeGreyAlpha, SizeRGB, SizeRGBA] =
        [X * Y * Comps || Comps <- lists:seq(1, 4)],
    ?assertMatch(<<_Pixels:SizeGrey/unit:8>>, Grey),
    ?assertMatch(<<_Pixels:SizeGreyAlpha/unit:8>>, GreyAlpha),
    ?assertMatch(<<_Pixels:SizeRGB/unit:8>>, RGB),
    ?assertMatch(<<_Pixels:SizeRGBA/unit:8>>, RGBA),

    % Ensure the alpha channel is 100%.
    ?assertMatch(<<0, 16#ff, _/binary>>, GreyAlpha),
    ?assertMatch(<<0, 0, 0, 16#ff, _/binary>>, RGBA).

% Test accessing a non-existent file.
enoent_test() ->
    Filename = filename_for(nonexistent, false),
    ?assertMatch({error, enoent}, stb_image:load(Filename)).

% Test reading a file with insufficient permissions.
eacces_test() ->
    {setup,
     fun () -> file:change_mode(filename_for(untouchable), 8#044) end,
     fun (_) -> file:change_mode(filename_for(untouchable), 8#644) end,
     fun () ->
             Filename = filename_for(untouchable),
             ?assertMatch({error, eacces}, stb_image:load(Filename))
     end
    }.

% Try to open a directory.
directort_test() ->
    % On Linux and OS X, opening a directory is a-okay 👌
    % So the error will be caught in stbi_load_from_file.
    ?assertMatch({error, {stb_image, _}}, stb_image:load(image_dir())).

%% Utils.

filename_for(Name) -> filename_for(Name, true).
filename_for(Name, Exists) ->
    BaseName = basename_for(Name),
    Path = filename:join(image_dir(), BaseName),
    Exists = filelib:is_file(Path),
    Path.

image_dir() ->
    case code:get_object_code(?MODULE) of
        error ->
            exit("Cannot find code object...");
        {_, _, ModPath} ->
            AppPath = filename:dirname(filename:dirname(ModPath)),
            filename:join(AppPath, "priv")
    end.

basename_for(any_image) -> "1px_256_table.png";
basename_for(xterm_table) -> "1px_256_table.png";
basename_for(nonexistent) -> "non-existent.png";
basename_for(untouchable) -> "hammer.gif".
