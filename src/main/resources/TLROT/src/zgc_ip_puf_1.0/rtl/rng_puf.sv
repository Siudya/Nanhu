// `timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 2023/03/29 15:45:14
// Design Name:
// Module Name: rng
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
module rng_puf(
    input clk,
    input request,
    input rst,
    output ready_challenge,
    output [127:0] challenge
);

parameter Challenge_repeat = 2;
//parameter initial_seed = 128'hC5141C0383F09022C512E99C9A2A50A4;
parameter initial_seed = 128'hC9F99D6C9F99D6C9F99D6C9F99D6C9F;

reg [127:0] seed;
reg ready_challenge_reg;

assign challenge = seed;

assign ready_challenge = ready_challenge_reg;

reg sig_delay;
reg out;

always @(posedge clk or negedge rst) begin
    if (!rst) begin
        out <= 0;
        sig_delay <= 0;
    end
    else begin
        sig_delay <= request;
        if (sig_delay == 0 && request == 1) begin
            out <= 1;
        end
        else begin
            out <= 0;
        end
    end
end

reg [5:0] request_cnt;

always @(posedge clk or negedge rst) begin
    if (!rst) begin
        seed <= initial_seed;
        ready_challenge_reg <= 1'd0;
        request_cnt <= 6'd0;
    end else if(out & request_cnt == Challenge_repeat-1) begin
        //seed[0] <= ~(seed[127] ^ seed[125] ^ seed[100] ^ seed[98]);
        //seed <= {seed[126:0],seed[0]};
        seed <= {seed[0]^seed[63]^seed[96]^seed[127],seed[127:1]};
        ready_challenge_reg <= 1'd1;
        request_cnt <= 6'd0;
    end else if (out) begin
        seed <= seed;
        ready_challenge_reg <= 1'd1;
        request_cnt <= request_cnt + 1'd1;
    end
    else begin
        seed <= seed;
        ready_challenge_reg <= 1'd0;
        request_cnt <= request_cnt;
     end
end

endmodule

