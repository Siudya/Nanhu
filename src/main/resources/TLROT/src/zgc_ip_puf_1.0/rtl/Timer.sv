// `timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 2023/03/29 16:58:55
// Design Name:
// Module Name: Timer
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


module Timer(
    input clk,
    input rst,
    input enable,
    output reg ctrl
);

parameter [5:0] target = 5'd15;
reg [31:0] count;

//reg sig_delay;
//reg out;

//always @(posedge clk or negedge rst) begin
//    if (!rst) begin
//        out <= 0;
//        sig_delay <= 0;
//    end
//    else begin
//        sig_delay <= enable;
//        if (sig_delay == 0 && enable == 1) begin
//            out <= 1;
//        end
//        else begin
//            out <= 0;
//        end
//    end
//end

always @(posedge clk or negedge rst) begin
    if (!rst) begin
        count <= 31'd0;
        ctrl <= 0;
    end else if (enable) begin
        if (count < target) begin
            count <= count + 1;
            ctrl <= 1;
        end else begin
            ctrl <= 0;
            count <= count;
        end
    end else if (!enable) begin
        ctrl <= 0;
        count <= 31'd0;
    end
end

endmodule

