-module(ulidb).

-export([generate/0,
         generate/1,
         generate_binary/0,
         generate_binary/1,
         encode/1,
         decode/1,
         from_uuid/1,
         to_uuid/1,
         extract_timestamp/1]).

-include("ulidb.hrl").

%%====================================================================
%% API
%%====================================================================
generate() ->
  Time = erlang:system_time(millisecond),
  generate(Time).

generate(Time) -> encode(generate_binary(Time)).

generate_binary() ->
  Time = erlang:system_time(millisecond),
  generate_binary(Time).

generate_binary(Time) ->
  case ulidb_store:get() of
    {Time, OldBytes} ->
      NewBytes = rotate(OldBytes),
      case ulidb_store:store(Time, OldBytes, NewBytes) of
        ok ->
          <<Time:48/unsigned-big, NewBytes/binary>>;
        retry -> %% Collission with a concurrent process
          generate_binary(Time)
      end;
    _ ->
      Bytes = crypto:strong_rand_bytes(10),
      ok = ulidb_store:store(Time, Bytes),
      <<Time:48/unsigned-big, Bytes/binary>>
  end.

extract_timestamp(<<Timestamp:48/unsigned-big, _:80/unsigned-big>>) -> Timestamp;
extract_timestamp(EncodedUlidb) ->
  <<Timestamp:48/unsigned-big, _:80/unsigned-big>> = decode(EncodedUlidb),
  Timestamp.

encode(<<_:128/unsigned-big>> = Ulid) -> encode(<<0:2, Ulid/binary>>, <<>>).

decode(Encoded) -> decode(Encoded, <<>>).

to_uuid(<<_:128/unsigned-big>> = Ulid) -> encode_uuid(Ulid);
to_uuid(Ulid) -> encode_uuid(decode(Ulid)).

from_uuid(Uuid) -> decode_uuid(Uuid).

%%====================================================================
%% Private
%%====================================================================
encode(<<>>, Acc) -> Acc;
encode(<<N:5, Rest/bitstring>>, Acc) when is_binary(Acc) ->
  <<_:N/binary, C:8, _/binary>> = ?CROCKFORD_ALPHABET,
  encode(Rest, <<Acc/bitstring, C>>).

decode(<<>>, <<_:2, Acc/bitstring>>) -> Acc;
decode(<<Char:8, Rest/binary>>, Acc) ->
  #{Char := Index} = ?CROCKFORD_INDEXED,
  decode(Rest, <<Acc/bitstring, Index:5>>).

encode_uuid(<<A1:4, A2:4, A3:4, A4:4, A5:4, A6:4, A7:4, A8:4, B1:4, B2:4, B3:4, B4:4,
              C1:4, C2:4, C3:4, C4:4, D1:4, D2:4, D3:4, D4:4, E1:4, E2:4, E3:4, E4:4, E5:4, E6:4,
              E7:4, E8:4, E9:4, E10:4, E11:4, E12:4>>) ->
  A1_ = e(A1),
  A2_ = e(A2),
  A3_ = e(A3),
  A4_ = e(A4),
  A5_ = e(A5),
  A6_ = e(A6),
  A7_ = e(A7),
  A8_ = e(A8),
  B1_ = e(B1),
  B2_ = e(B2),
  B3_ = e(B3),
  B4_ = e(B4),
  C1_ = e(C1),
  C2_ = e(C2),
  C3_ = e(C3),
  C4_ = e(C4),
  D1_ = e(D1),
  D2_ = e(D2),
  D3_ = e(D3),
  D4_ = e(D4),
  E1_ = e(E1),
  E2_ = e(E2),
  E3_ = e(E3),
  E4_ = e(E4),
  E5_ = e(E5),
  E6_ = e(E6),
  E7_ = e(E7),
  E8_ = e(E8),
  E9_ = e(E9),
  E10_ = e(E10),
  E11_ = e(E11),
  E12_ = e(E12),
  <<A1_, A2_, A3_, A4_, A5_, A6_, A7_, A8_, $-, B1_, B2_, B3_, B4_, $-, C1_, C2_, C3_, C4_,
    $-, D1_, D2_, D3_, D4_, $-, E1_, E2_, E3_, E4_, E5_, E6_, E7_, E8_, E9_, E10_, E11_,
    E12_>>.

e(0) -> $0;
e(1) -> $1;
e(2) -> $2;
e(3) -> $3;
e(4) -> $4;
e(5) -> $5;
e(6) -> $6;
e(7) -> $7;
e(8) -> $8;
e(9) -> $9;
e(10) -> $a;
e(11) -> $b;
e(12) -> $c;
e(13) -> $d;
e(14) -> $e;
e(15) -> $f.

decode_uuid(<<A1, A2, A3, A4, A5, A6, A7, A8, $-, B1, B2, B3, B4, $-, C1, C2, C3, C4, $-,
              D1, D2, D3, D4, $-, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12>>) ->
  A1_ = d(A1),
  A2_ = d(A2),
  A3_ = d(A3),
  A4_ = d(A4),
  A5_ = d(A5),
  A6_ = d(A6),
  A7_ = d(A7),
  A8_ = d(A8),
  B1_ = d(B1),
  B2_ = d(B2),
  B3_ = d(B3),
  B4_ = d(B4),
  C1_ = d(C1),
  C2_ = d(C2),
  C3_ = d(C3),
  C4_ = d(C4),
  D1_ = d(D1),
  D2_ = d(D2),
  D3_ = d(D3),
  D4_ = d(D4),
  E1_ = d(E1),
  E2_ = d(E2),
  E3_ = d(E3),
  E4_ = d(E4),
  E5_ = d(E5),
  E6_ = d(E6),
  E7_ = d(E7),
  E8_ = d(E8),
  E9_ = d(E9),
  E10_ = d(E10),
  E11_ = d(E11),
  E12_ = d(E12),
  <<A1_:4, A2_:4, A3_:4, A4_:4, A5_:4, A6_:4, A7_:4, A8_:4, B1_:4, B2_:4, B3_:4, B4_:4,
    C1_:4, C2_:4, C3_:4, C4_:4, D1_:4, D2_:4, D3_:4, D4_:4, E1_:4, E2_:4, E3_:4, E4_:4, E5_:4,
    E6_:4, E7_:4, E8_:4, E9_:4, E10_:4, E11_:4, E12_:4>>.

d($0) -> 0;
d($1) -> 1;
d($2) -> 2;
d($3) -> 3;
d($4) -> 4;
d($5) -> 5;
d($6) -> 6;
d($7) -> 7;
d($8) -> 8;
d($9) -> 9;
d($A) -> 10;
d($B) -> 11;
d($C) -> 12;
d($D) -> 13;
d($E) -> 14;
d($F) -> 15;
d($a) -> 10;
d($b) -> 11;
d($c) -> 12;
d($d) -> 13;
d($e) -> 14;
d($f) -> 15.

rotate(Bytes) ->
  Bits = deconstruct(Bytes),
  reconstruct(do_rotate(rev(Bits), {Bytes, <<>>})).

do_rotate(<<X:8, Rest/binary>>, {OrigBytes, FlippedBytes}) ->
  case X of
    31 -> do_rotate(Rest, {OrigBytes, <<FlippedBytes/binary, 0>>});
    _ -> rev(<<FlippedBytes/binary, (X + 1), Rest/binary>>)
  end.

rev(Bytes) ->
  Size = erlang:size(Bytes) * 8,
  <<X:Size/integer-little>> = Bytes,
  <<X:Size/integer-big>>.

deconstruct(Bytes) -> deconstruct(Bytes, <<>>).

deconstruct(<<>>, Acc) -> Acc;
deconstruct(<<X:5, Rest/bitstring>>, Acc) -> deconstruct(Rest, <<Acc/bitstring, X>>).

reconstruct(Bytes) -> reconstruct(Bytes, <<>>).

reconstruct(<<>>, Acc) -> Acc;
reconstruct(<<X:8, Rest/bitstring>>, Acc) -> reconstruct(Rest, <<Acc/bitstring, X:5>>).
