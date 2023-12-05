-module(csv_reader).
-export([read_csv_file/1, print_csv_data/1]).

read_csv_file(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    Data = read_lines(Device, []),
    file:close(Device),
    Data.

read_lines(Device, Acc) ->
    case file:read_line(Device) of
        {ok, Line} ->
            % Split the line by comma (CSV delimiter)
            Columns = string:tokens(Line, ",\n"),
            read_lines(Device, [Columns | Acc]);
        eof ->
            lists:reverse(Acc)
    end.

print_csv_data([]) ->
    ok;
print_csv_data([Row | Rows]) ->
    io:format("~p~n", [Row]),
    print_csv_data(Rows).
