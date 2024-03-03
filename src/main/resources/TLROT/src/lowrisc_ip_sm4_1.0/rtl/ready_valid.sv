module ready_valid (
    input wire clk,
    input wire ready_out,
    input wire encdec_enable_in,
    output reg valid_out
);

always @(posedge clk) begin
    if (ready_out == 1) begin
        valid_out <= 1; // 当ready_out信号为高电平时，A信号为高电平
    end
    else if (encdec_enable_in == 0) begin
        valid_out <= 0; // 当encdec_enable_in信号为低电平时，A信号为低电平
    end
end

endmodule