// `timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 2023/03/30 10:46:11
// Design Name:
// Module Name: Comp
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


module Comp(
    input clk,
    input rst,
    input [31:0] num1,
    input [31:0] num2,
    input comp_en,
    output reg done,
    output reg [1:0] result
);

//can add a mode signle to change comp mode
reg [31:0] num1_reg;
reg [31:0] num2_reg;

always @(posedge clk or negedge rst) begin
    if(!rst) begin
        done <= 0;
        result <= 2'd0;
        num1_reg <= 32'd0;
        num2_reg <= 32'd0;
    end else begin
        if(comp_en) begin
            num1_reg <= num1;
            num2_reg <= num2;
            if(num1 > num2) begin
                done <= 1;
                result <= 2'b11;
            end else if(num1 == num2) begin
                done <= 1;
                result <= 2'b01;
            end else begin
                done <= 1;
                result <= 2'b00;
            end
        end else begin
            done <= 0;
            result <= 2'b00;
            num1_reg <= 32'd0;
            num2_reg <= 32'd0;
        end
    end
end

endmodule



