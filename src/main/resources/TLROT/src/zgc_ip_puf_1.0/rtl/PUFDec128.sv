// `timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 2023/03/29 15:38:12
// Design Name:
// Module Name: PUFDec128
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


module PUFDec128(
    input [6:0] i_Sel,
    output reg [127:0] o_Q
);

generate
    genvar i;
    for(i=0; i<128; i=i+1) begin
        always @ (i_Sel) begin
            if(i == i_Sel) begin
                o_Q[i] = 1'b1;
            end else begin
                o_Q[i] = 1'b0;
            end
        end
    end
endgenerate

endmodule

