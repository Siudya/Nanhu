// `timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 2023/03/29 14:50:53
// Design Name:
// Module Name: PUF128
// Project Name:
// Target Devices:
// Tool Versions:
// Description:
//
// Dependencies:
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////

//`define WIDTH  256
`include "header.sv"

module PUF128(
    input clk,
    input rst,
    input enable,             //PUF start
    input mode,              // 0: rng mode; 1: puf mode;
    input [127:0] challenge,
    output request,          //request a new challenge for rng mode
    input ready_challenge,   //new challenge ready
    output response_done,             //response generated
    output [`WIDTH - 1:0] response,
    output response_done_2bit,
    output [1:0] response2bit
);

parameter VOTE_NUM = 10;
// parameter CHALLENGE_NUM = 1000;

reg timer_enable;          //timer start to work
reg request_reg;
reg [8:0] shift_cnt;       // shift for 128 cycle
reg [5:0] vote_cnt;        //num of one response collect
reg [127:0] challenge_reg;
// wire response_done_2bit;   //PUF generate one bit response
// wire [1:0] response2bit;
reg [`WIDTH - 1:0] response_one;  //one 128bit response
reg one_resp_ready;        //one 128bit response ready to write to voter
reg vote_start;            //start to vote response
reg vote_clear;            //clear data in voter
//wire response_done;        //response after voting done
//reg [ `WIDTH - 1 :0] response_reg;  //final output response
reg [3:0] state;            // state reg for FSM

localparam                       IDLE =  0;            //wait for PUF start
localparam                       REQUEST  =  1;        //request new challenge for rng mode
localparam                       UPDATE = 2;           //challenge upated for rng mode
localparam                       RNG = 3;              //rng output gen
localparam                       SHIFT = 4;            //shift challenge (128)
localparam                       VOTE_STORE = 5;       //writ data to voter
localparam                       VOTE = 6;              //start vote
localparam                       ALL_DONE = 7;          //all tings done


PUF1bit puf_inst (
    .clk(clk),
    .rst(rst),
    .timer_enable(timer_enable),
    .challenge(challenge_reg),
    .response_done(response_done_2bit),
    .response(response2bit)
);

//vote
Voter #(.Threshold(5)) u_voter(
  .clk(clk),
  .rst(rst),
  .ready(one_resp_ready),
  .vote(vote_start),
  .clear(vote_clear),
  .done(response_done),
  .data_in(response_one),
  .data_out(response)
);

assign request = request_reg;
//assign response = response_reg;

always @(posedge clk or negedge rst) begin
    if (!rst) begin
        state <= IDLE;
        timer_enable <= 1'd0;
        request_reg <= 1'd0;
        shift_cnt <= 9'd0;
        vote_start <= 1'd0;
        vote_clear <= 1'd0;
        one_resp_ready <= 1'd0;
        vote_cnt <= 6'd0;
        response_one <= `WIDTH'd0;
        challenge_reg <= 128'd0;
    end else begin
        case(state)
            IDLE: begin
                if (enable) begin
                    if (!mode) begin              // jump to rng mode, request new challenge
                        state <= REQUEST;
                        timer_enable <= 1'd0;
                        request_reg <= 1'd0;
                        shift_cnt <= 9'd0;
                        vote_start <= 1'd0;
                        vote_clear <= 1'd0;
                        one_resp_ready <= 1'd0;
                        vote_cnt <= 6'd0;
                        response_one <= `WIDTH'd0;
                        challenge_reg <= 128'd0;
                    end else if (mode & (challenge != 0)) begin              // jump to puf mode, use challenge from primary input
                        state <= SHIFT;
                        timer_enable <= 1'd0;
                        request_reg <= 1'd0;
                        shift_cnt <= 9'd0;
                        vote_start <= 1'd0;
                        vote_clear <= 1'd0;
                        one_resp_ready <= 1'd0;
                        vote_cnt <= 6'd0;
                        response_one <= `WIDTH'd0;
                        challenge_reg <= challenge;
                    end
                end
            end
            REQUEST: begin
                if (enable & !mode) begin
                    state <= UPDATE;
                    timer_enable <= 1'd0;
                    request_reg <= 1'd1;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd1;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    challenge_reg <= 128'd0;
                end else begin
                    state <= ALL_DONE;
                    timer_enable <= 1'd0;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    challenge_reg <= 128'd0;
                end
            end
            UPDATE: begin
                if (ready_challenge) begin
                    state <= RNG;
                    timer_enable <= 1'd0;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    challenge_reg <= challenge;
                end
            end
            RNG: begin
                if (!response_done_2bit) begin
                    timer_enable <= 1'd1;
                    challenge_reg <= challenge_reg;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    state <= RNG;
                end else if (response_done_2bit) begin
                    timer_enable <= 1'd0;
                    challenge_reg <= challenge_reg;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    state <= REQUEST;
                end
            end
            SHIFT: begin
                if (shift_cnt < 9'd128) begin
                    if (!response_done_2bit) begin
                        timer_enable <= 1'd1;
                        challenge_reg <= challenge_reg;
                        request_reg <= 1'd0;
                        shift_cnt <= shift_cnt;
                        vote_start <= 1'd0;
                        vote_clear <= 1'd0;
                        one_resp_ready <= 1'd0;
                        vote_cnt <= vote_cnt;
                        response_one <= response_one;
                        state <= SHIFT;
                    end else if (response_done_2bit & timer_enable) begin
                        timer_enable <= 1'd0;
                        challenge_reg <= {challenge_reg[126:0], challenge_reg[127]};
                        request_reg <= 1'd0;
                        shift_cnt <= shift_cnt + 1'd1;
                        vote_start <= 1'd0;
                        vote_clear <= 1'd0;
                        one_resp_ready <= 1'd0;
                        vote_cnt <= vote_cnt;
                        response_one <= {response_one[`WIDTH-3:0],response2bit};
                        state <= SHIFT;
                    end else begin
                        timer_enable <= timer_enable;
                        challenge_reg <= challenge_reg;
                        request_reg <= 1'd0;
                        shift_cnt <= shift_cnt;
                        vote_start <= 1'd0;
                        vote_clear <= 1'd0;
                        one_resp_ready <= 1'd0;
                        vote_cnt <= vote_cnt;
                        response_one <= response_one;
                        state <= SHIFT;
                    end
                end else begin
                    timer_enable <= 1'd0;
                    challenge_reg <= challenge_reg;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd1;
                    vote_cnt <= vote_cnt;
                    response_one <= response_one;
                    state <= VOTE_STORE;
                end
            end
            VOTE_STORE: begin
                if (vote_cnt < VOTE_NUM -1 ) begin
                    timer_enable <= 1'd0;
                    challenge_reg <= challenge_reg;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= vote_cnt + 1'd1;
                    response_one <= `WIDTH'd0;
                    state <= SHIFT;
                end else if (vote_cnt >= VOTE_NUM -1 ) begin
                    timer_enable <= 1'd0;
                    challenge_reg <= challenge_reg;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd1;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    state <= VOTE;
                end
            end
            VOTE: begin
                if (response_done) begin
                    timer_enable <= 1'd0;
                    challenge_reg <= challenge_reg;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    state <= ALL_DONE;
                end else begin
                    timer_enable <= 1'd0;
                    challenge_reg <= challenge_reg;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd1;
                    vote_clear <= 1'd0;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    state <= VOTE;
                end
            end
            ALL_DONE: begin
                if (enable) begin
                    state <= ALL_DONE;
                    timer_enable <= 1'd0;
                    request_reg <= 1'd0;
                    shift_cnt <= 9'd0;
                    vote_start <= 1'd0;
                    vote_clear <= 1'd1;
                    one_resp_ready <= 1'd0;
                    vote_cnt <= 6'd0;
                    response_one <= `WIDTH'd0;
                    challenge_reg <= 128'd0;
                end else begin
                    state <= IDLE;
                end
            end

            default: state <= IDLE;
        endcase
    end
end

endmodule

