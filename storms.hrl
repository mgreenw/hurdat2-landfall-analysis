%%%
%%% storms.hrl
%%% Author: Max Greenwald
%%% Date: October 19, 2018
%%%
%%% Define the storm and track_point records for use in storm
%%% parsing and analysis
%%%

-record(storm,
  {
    basin,
    cyclone_id,
    year,
    name,
    track_points = []
  }
).

-record(track_point,
  {
    year,
    month,
    day,
    hours,
    minutes,
    record_id,
    system_status,
    latitude,
    longitude,
    max_sustained_wind,
    min_pressure,
    radius
  }
).
