`include "header.sv"

module PUF_core (
    input clk,
    input rst_n,
    input enable,
    input mode,   // 0: rng mode; 1: puf mode
    input ready_challenge,  //new challenge ready
    input [127:0] challenge, // a 128bit challenge
    output response_done,
    output response_valid,
    output [`WIDTH - 1:0] response,
    output response_valid_re,
    output [`WIDTH/32 -1:0]response_re,
    output response_done_2bit_re,
    output response_done_2bit,
    output [1:0] response2bit
);

wire request;
wire [127:0] challenge_rng, challenge_puf;
wire ready_challenge_rng, ready_challenge_puf;
wire response_done_2bit_puf;
wire [1:0] response2bit_puf;

assign challenge_puf = mode ? challenge : challenge_rng;
assign ready_challenge_puf = mode ? ready_challenge : ready_challenge_rng;

assign response_done_2bit = mode ? 0 : response_done_2bit_puf;
assign response2bit = mode ? 0 : response2bit_puf;

assign response_valid_re = 1;
assign response_re = {8{response_done}};
assign response_done_2bit_re = 1;
PUF128 #(.VOTE_NUM(10)) puf_128 (
    .clk(clk),
    .rst(rst_n),
    .enable(enable),
    .mode(mode),
    .challenge(challenge_puf),
    .ready_challenge(ready_challenge_puf),
    .response_done(response_done),
    .response(response),
    .request(request),
    .response_done_2bit(response_done_2bit_puf),
    .response2bit(response2bit_puf)
);

rng_puf #(.Challenge_repeat(1)) u_rng(
  .clk(clk),                // Clock input
  .request(request),        // Request input
  .rst(rst_n),                // Reset input
  .ready_challenge(ready_challenge_rng),  // Ready output
  .challenge(challenge_rng)     // Challenge output
);

AP0BP1 u_AP0BP1(
  .clk   (clk   ),
  .rst_n (rst_n ),
  .A     (ready_challenge),
  .B     (response_done),
  .C     (response_valid)
);

endmodule

