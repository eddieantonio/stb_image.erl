-module(stb_image_tests).

-include_lib("eunit/include/eunit.hrl").

% Test loading any regular image.
load_test() ->
    Filename = filename_for(any_image),
    ?assertMatch({ok, _Image}, stb_image:load(Filename)).

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

%% Utils.

filename_for(Name) -> filename_for(Name, true).
filename_for(Name, Exists) ->
    BaseName = basename_for(Name),
    Path = filename:join(image_dir(), BaseName),
    io:format("Looking for file ~s~n", [Path]),
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
basename_for(nonexistent) -> "non-existent.png";
basename_for(untouchable) -> "hammer.gif".
