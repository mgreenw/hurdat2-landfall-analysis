%%%
%%% storm_parser.erl
%%% Author: Max Greenwald
%%% Date: October 19, 2018
%%%
%%% Parser for the HurDat2 data format. Create storm records and send
%%% storms that have been successfully parsed to the analyzer.
%%%

-module(storm_parser).

-include_lib("storms.hrl").
-export([start/2]).

%% Start the storm parser given a HurDat2 Filename and Analyzer PID
start(Filename, Analyzer) ->
  {ok, HurDat2} = file:open(Filename, [read]),
  read_storms(HurDat2, Analyzer).

%% Read all of the storms from the file. Send each storm since 1900
%% to the analyzer, then read the next storm
read_storms(File, Analyzer) ->
  case read_storm(File) of
    done -> ok;
    {error, Reason} ->
      % Print the error and try the next storm
      io:format("Could not read storm: ~p~n", [Reason]),
      read_storms(File, Analyzer);
    Storm ->
      % We only want to get storms since 1900
      case Storm#storm.year >= 1900 of
        true -> Analyzer ! {storm, Storm};
        _ -> ok
      end,
      read_storms(File, Analyzer)
  end.

%% Read a single storm from the file. Return the storm record.
read_storm(File) ->
  case io:get_line(File, "") of

    % End of file, no more work to do.
    eof  -> done;

    % A new line of the file! It should be a new storm header of length 4
    Line ->

      % Clean the input, split it by commas
      Fields = clean_fields(Line),

      % Check the number of fields. A storm header should have 4 fields
      case length(Fields) of
        4 ->

          % Get the raw data from the fields
          [CycloneInfo, Name, BestTrackEntryCount | _] = Fields,

          % Parse the storm data into a storm record and return it.
          #storm{
            basin = string:slice(CycloneInfo, 0, 2),
            cyclone_id = string:slice(CycloneInfo, 2, 2),
            year = str_to_int(string:slice(CycloneInfo, 4, 4)),
            name = Name,

            % Get all of the track points from the file
            % Use the provided count to limit best track rows
            track_points = read_track_points(File, str_to_int(BestTrackEntryCount))
          };

        % If the row is not a storm header, return an error
        FieldCount ->
          io:format("Count not read row with ~p fields~n", [FieldCount]),
          {error, toomanyfields}
      end
    end.

%% Recursively read a set number of Track Points from the file. Return
%% a list of the point records
read_track_points(_, 0) -> []; % If no points remaining, then return empty list
read_track_points(File, PointsRemaining) ->

  % Get a row from the file.
  case io:get_line(File, "") of

    % If eof, no more work to do.
    eof -> [];

    % Given a line of data, check that the data is a valid Track Point
    Line ->
      Fields = clean_fields(Line),
      case length(Fields) of

        % Track points should have 21 fields. Otherwise, error
        21 ->

          % Get fields from the row
          [Date, Time, Record, Status, VerticalPosition, HorizontalPosition, MaxWind, MinPressure | WindRadii] = Fields,

          % Calculate the radius of the storm for this track point as the max
          % distance with wind over 34 knots.
          Radii = lists:map(fun(Radius) -> float(str_to_int(Radius)) end, lists:sublist(WindRadii, 12)),
          Radius = case lists:max(Radii) of
            R when R > 0 -> R;
            _ -> 100.0
          end,

          % Make the track point record
          Point = #track_point{
            year = str_to_int(string:slice(Date, 0, 4)),
            month = str_to_int(string:slice(Date, 4, 2)),
            day = str_to_int(string:slice(Date, 6, 2)),
            hours = str_to_int(string:slice(Time, 0, 2)),
            minutes = str_to_int(string:slice(Time, 2, 2)),
            record_id = string:slice(Record, 0, 1),
            system_status = string:slice(Status, 0, 2),
            latitude = str_to_float(string:slice(VerticalPosition, 0, 5)),
            longitude = str_to_float(string:slice(HorizontalPosition, 0, 5)),
            max_sustained_wind = str_to_int(MaxWind),
            min_pressure = str_to_int(MinPressure),
            radius = Radius / 55.0 % Roughly convert miles to degrees of lat/lon
          },

          % Prepend the point to the array and recurse on the remaining points
          [Point | read_track_points(File, PointsRemaining - 1)];

        % On error (incorrect number of fields), log error and try next row
        FieldCount ->
          io:format("Could not read entry with ~p fields~n", [FieldCount]),
          read_track_points(File, PointsRemaining - 1)
      end
  end.

%%
%% Helper functions
%%

%% Clean a line of CSV input: separate by commas, strip each string
clean_fields(Line) ->
  lists:map(
    fun(Elem) -> string:strip(Elem) end,
    string:split(Line, ",", all)
  ).

%% Cast string to an int. Return 0 on error
str_to_int(String) ->
  case string:to_integer(String) of
    {error, _} ->
      io:format("Error reading int from string ~p~n.", [String]),
      0;
    {Int, _} -> Int
  end.

%% Cast string to float. Return 0.0 on error.
str_to_float(String) ->
  case string:to_float(String) of
    {error, _} ->
      io:format("Error reading float from string ~p~n.", [String]),
      0.0;
    {Float, _} -> Float
  end.
