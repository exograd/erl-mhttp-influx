%% Copyright (c) 2022 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mhttp_influx).

-export([client_request_hook/4, server_request_hook/5]).

-spec client_request_hook(mhttp:request(), mhttp:response(), integer(),
                          mhttp:pool_id()) ->
        ok.
client_request_hook(Request = #{method := Method},
                    Response = #{status := Status},
                    RequestTime, PoolId) ->
  ReqBody = mhttp_request:body(Request),
  ResBody = mhttp_response:body(Response),
  Tags =
    #{pool => PoolId},
  Fields =
    #{method => Method,
      req_body_size => iolist_size(ReqBody),
      res_body_size => iolist_size(ResBody),
      status => Status,
      req_time => RequestTime},
  Point = influx:point(mhttp_outgoing_requests, Fields, Tags),
  influx:enqueue_point(default, Point),
  ok.

-spec server_request_hook(mhttp:request(), mhttp:response(), integer(),
                          binary(), mhttp:pool_id()) ->
        ok.
server_request_hook(Request = #{method := Method},
                    Response = #{status := Status},
                    RequestTime, RouteId, ServerId) ->
  ReqBody = mhttp_request:body(Request),
  ResBody = mhttp_response:body(Response),
  Tags =
    #{server => ServerId,
      route => RouteId,
      status => integer_to_binary(Status)},
  Fields =
    #{method => Method,
      req_body_size => iolist_size(ReqBody),
      res_body_size => iolist_size(ResBody),
      req_time => RequestTime},
  Point = influx:point(mhttp_incoming_requests, Fields, Tags),
  influx:enqueue_point(default, Point),
  ok.
