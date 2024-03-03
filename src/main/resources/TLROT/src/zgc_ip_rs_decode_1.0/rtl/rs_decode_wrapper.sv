module rs_decode_wrapper(
    input clk,
    input rst_n, // Active low reset
    input decode_en,
    input clrn,
    input [200*8-1:0] encoded_data,
    output reg [200*8-1:0] error_pos,
    output reg output_valid,
    output reg ready,
    output  with_error,  //output reg  with_error 
    
    output  ready_re,
    output  output_valid_re,
    output  [49:0] error_pos_re,
    output  with_error_re
);

// State declaration
localparam [1:0]
    IDLE = 2'b00,
    DECODE = 2'b01,
    COLLECT_ERROR = 2'b10,
    COMPLETE = 2'b11;

// Internal signals
reg [7:0] received;
reg dec_ena;
wire [7:0] error;
wire valid;
reg [11:0] bit_count;
reg [1:0] state, next_state;
reg [7:0] decode_counter; // Counter to maintain dec_ena for k clock cycles

localparam [7:0] k = 8'd200;

assign ready_re = 1'b1;
assign output_valid_re = output_valid;
assign with_error_re = 1'b1;
assign error_pos_re = {50{output_valid}};

// Instantiate the rsdec decoder module
rsdec x2 (
    .x(received),
    .error(error),
    .with_error(with_error), // Assuming not used, connect it properly if required
    .enable(dec_ena), // Connect dec_ena signal
    .valid(valid),
    .k(k),
    .clk(clk),
    .clrn(rst_n & clrn)
    // Other connections to rsdec if needed
);

// State machine transition and output logic
always @(posedge clk or negedge rst_n) begin
    if (!rst_n | !clrn) begin
        state <= IDLE;
        bit_count <= 0;
        error_pos <= 0;
        output_valid <= 0;
        decode_counter <= 0;
        dec_ena <= 0;
        ready <= 1;
    end else begin
        case (state)
            IDLE: begin
                if (decode_en) begin
                    state <= DECODE;
                    dec_ena <= 0; // Enable the decoder
                    decode_counter <= 0; // Reset the counter
                    bit_count <= 0;
                    ready <= 0;
                end
            end
            DECODE: begin
                if (decode_counter >= (k)) begin
                    dec_ena <= 0;
                end else begin
                    dec_ena <= 1;
                    received <= encoded_data[decode_counter*8 +: 8]; // Grab the next byte of input data
                    decode_counter <= decode_counter + 1; // Increment counter
                end

                if (!dec_ena && with_error) begin
                    state <= COLLECT_ERROR;
                    decode_counter <= 0;
                end
            end
            COLLECT_ERROR: begin
                if (valid) begin
                    error_pos[bit_count*8 +: 8] <= error; // Collect error data
                    bit_count <= bit_count + 1; // Increment bit_count to point to the next byte
                    if (bit_count >= k-1) begin // Check if all bytes processed
                        state <= COMPLETE;
                    end
                end else begin
                    state <= COLLECT_ERROR; // Transition to complete once valid goes low
                end
            end
            COMPLETE: begin
                output_valid <= 1; // Indicate that the output data is valid
                state <= IDLE; // Reset to idle state for the next operation
                bit_count <= 0; // Reset bit_count for the next operation
                ready <= 1;
            end
            default: state <= IDLE;
        endcase
    end
end

endmodule

