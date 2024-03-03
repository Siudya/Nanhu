// `timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 2023/03/30 11:13:19
// Design Name:
// Module Name: Vote
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

`include "header.sv"

module Voter(
  input wire clk,
  input wire rst,
  input wire ready,
  input wire vote,
  input wire clear,
  output reg done,
  input wire [`WIDTH-1:0] data_in,
  output reg [`WIDTH-1:0] data_out
);

parameter  Threshold = 5; // ��ֵ

//reg [127:0] data_out_reg;
reg [5:0] sum [0:`WIDTH-1];

genvar i;
generate
  for (i = 0; i < `WIDTH; i = i + 1) begin : adder
    always@(posedge clk or negedge rst) begin
        if(!rst) begin
            sum[i] <= 0;
            data_out[i] <= 0;
            done <= 1'd0;
        end else if (ready) begin
            sum[i] <= data_in[i] + sum[i];
            data_out[i] <= 0;
            done <= 1'd0;
        end else if (vote) begin
            if ( sum[i] >= Threshold ) begin
                data_out[i] <= 1;
                done <= 1'd1;
            end else begin
                data_out[i] <= 0;
                done <= 1'd1;
            end
        end else if (clear) begin
            sum[i] <= 0;
            data_out[i] <= 0;
            done <= 1'd0;
        end else begin
            sum[i] <= sum[i];
            data_out[i] <= data_out[i];
            done <= done;
        end
      end
    end

endgenerate

//assign data_out = data_out_reg;

endmodule

