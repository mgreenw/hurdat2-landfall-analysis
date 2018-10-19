%%%
%%% analysis_worker.erl
%%% Author: Max Greenwald
%%% Date: October 19, 2018
%%%
%%% Worker for the storm analyzer. Compute if a storm intersects the
%%% given regions, and send the result back to the leader.
%%%

-module(analysis_worker).

-include_lib("storms.hrl").
-export([worker_loop/2]).

%% Main worker loop. Wait to receive an incoming storm, then analyze it.
%% On completion of analysis, alert the leader that worker is on standby
%% and ready to receive another storm
worker_loop(Leader, Regions) ->
  receive

    % Receive a storm!
    {storm, Storm} ->

      % Check if a storm intersects the given regions.
      case storm_intersects_regions(Storm#storm.track_points, Regions) of

        % If so, get the max windspeed for the event and the date of
        % landfall and alert the leader
        {true, Date} ->
          WindSpeeds = lists:map(fun(TrackPoint) -> TrackPoint#track_point.max_sustained_wind end, Storm#storm.track_points),
          MaxWindSpeed = lists:max(WindSpeeds),
          Leader ! {print, {Storm#storm.name, Date, MaxWindSpeed}};

        % If the storm never makes landfall, don't do anything
        false ->
          ok
      end,

      % Tell the leader that the worker is ready and loop
      Leader ! {worker_ready, self()},
      worker_loop(Leader, Regions);

    % Stop the worker: don't loop.
    {stop} ->
      ok
  end.

%% Check if a storm intersects any of the regions in the regions list.
%% Recurse on the the list of Track Points in the storm. Return true
%% on the first track point that intersects the regions (makes landfall).
storm_intersects_regions([], _) -> false;
storm_intersects_regions([TrackPoint | TrackPoints], Regions) ->

  % Get the storm's center and radus from the track point
  Center = {TrackPoint#track_point.latitude, TrackPoint#track_point.longitude},
  Radius = TrackPoint#track_point.radius,

  % Check if the storm's radius intersects with one of the regions defined
  case circle_intersects_regions(Center, Radius, Regions) of

    % If the track point intersects the regions, return true! Generate the date
    true ->
      Date = {
        TrackPoint#track_point.year,
        TrackPoint#track_point.month,
        TrackPoint#track_point.day,
        TrackPoint#track_point.hours,
        TrackPoint#track_point.minutes
      },
      {true, Date};

    % If the thrack point does not intersect the regions, recurse
    _ ->
      storm_intersects_regions(TrackPoints, Regions)
  end.

%% Check if a circle intersects a list of regions. Recurse over every region
circle_intersects_regions(_, _, []) -> false;
circle_intersects_regions(Center, Radius, [Region | Regions]) ->

  % True of either the circle intersects this region or another of the regions
  circle_intersects_region(Center, Radius, Region) or
    circle_intersects_regions(Center, Radius, Regions).

%% Check if a circle intersects a single region (polygon as list of points).
%% There are two cases. 1) Any of the region's points are within the radius
%% of the circle. If so, return true. Otherwise, 2) the circle could be
%% completely within the region. If so, then a ray from the center of the
%% circle in any direction will intersect with an odd number of edges of the
%% region. If the intersection count is odd, then the circle intersects the
%% region. Otherwise, the circle is outside of the region.
circle_intersects_region(Center, Radius, Region) ->

  % Case 1) One of the region's point is inside the circle
  RegionPointInRadius = lists:any(
    fun(E) -> E end,
    lists:map(
      fun(RegionPoint) -> point_in_circle(RegionPoint, Center, Radius) end,
      Region
    )
  ),

  % or Case 2) The circle is inside the region (# of intersections is odd)
  RegionPointInRadius or ((ray_intersections(Center, Region) rem 2) /= 0).

%% Check if a point is within the cicle
point_in_circle({Point_x, Point_y}, {Circle_x, Circle_y}, Radius) ->

  % the distance from the point to the center of the circle will be less
  % than the radius if the point is in the circle
  Distance = math:sqrt(math:pow(Point_x - Circle_x, 2) + math:pow(Point_y - Circle_y, 2)),
  Distance =< Radius.

%% Recursively find the numer of intersections of a ray from a point pointed
%% east and the endges in a region.
ray_intersections(Point, [RegionPoint1, RegionPoint2 | Region]) ->

  % Check if the ray and a line segment intersect
  SegmentsIntersect =
    line_segments_intersect(
      {RegionPoint1, diff(RegionPoint2, RegionPoint1)},
      {Point, {60, 30}}
    ),

  % Recursively add the result of the intersections (1 per ray/segment combo)
  ray_intersections(Point, [RegionPoint2 | Region]) +
    case SegmentsIntersect of
      true -> 1;
      _ -> 0
    end;
%% Recursive base case. Needs to be a t the end so it doesn't match everything.
ray_intersections(_, [_ | _]) -> 0.

%% Check if two line segments between {P, P+R} and {Q, Q+S} intersect.
%% Use
line_segments_intersect({P, R}, {Q, S}) ->

  % Calculate cross products to check if parallel and/or intersecting
  Parallel = cross(R, S),
  Intersecting = cross(diff(Q, P), R),
  case {Parallel, Intersecting} of

    % The two segments are collinear. Check if any of the end points are equal
    % or if the differences between the points are not all negative or not all
    % positive
    {0.0, 0.0} ->
      PointsEqual = (P == Q) or (sum(P, R) == Q) or (R == sum(Q, S)) or (sum(P, R) == sum(Q, S)),
      case PointsEqual of
        true -> true;
        _ ->
          Differences = [diff(Q, P), diff(Q, sum(P, R)), diff(sum(Q, S), P), diff(sum(Q, S), sum(P, R))],
          not lists:all(fun({X_Diff, Y_Diff}) -> (X_Diff < 0) and (Y_Diff < 0) end, Differences)
      end;

    % The two segments are Pparallel but don't intersect
    {0.0, _} -> false;

    % Calculate T and V, the quotiont of the two cross products of each point
    % and the other vector. If either are between 0 and 1, then
    % the segments intersect.
    _ ->
      T = cross(diff(Q, P), S) / cross(R, S),
      U = cross(diff(Q, P), R) / cross(R, S),
      between_zero_and_one(T) and between_zero_and_one(U)
  end.

%% Helper functions
between_zero_and_one(X) -> (X =< 1) and (X >= 0).
sum({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.
diff({X1, Y1}, {X2, Y2}) -> {X1 - X2, Y1 - Y2}.
cross({X1, Y1}, {X2, Y2}) -> (X1 * Y2) - (Y1 * X2).
