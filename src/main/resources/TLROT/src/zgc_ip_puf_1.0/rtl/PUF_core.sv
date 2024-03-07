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
    output [1:0] response2bit,
    output [3:0] rng4bit,
    output rng4bit_done,
    output rng_mode,
    input es_rng_req
);

wire request;
wire [127:0] challenge_rng, challenge_puf;
wire ready_challenge_rng, ready_challenge_puf;
wire response_done_2bit_puf;
wire [1:0] response2bit_puf;

reg [3:0] rng4bit_reg;
reg rng4bit_done_reg;

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

assign rng_mode = mode;

// Internal signals
reg [1:0] counter; // 2-bit counter to count the number of times response2bit_puf is received
reg response_done_2bit_puf_d; // Delayed version of response_done_2bit_puf for edge detection

// Edge detection for response_done_2bit_puf
wire response_done_2bit_puf_rising;
assign response_done_2bit_puf_rising = response_done_2bit_puf & ~response_done_2bit_puf_d;

assign rng4bit = es_rng_req ? rng4bit_reg : 0;
assign rng4bit_done = rng4bit_done_reg & es_rng_req;

// Sequential logic for updating rng4bit_reg and rng4bit_reg_done
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        // Reset state
        rng4bit_reg <= 4'b0000;
        rng4bit_done_reg <= 1'b0;
        counter <= 2'b00;
        response_done_2bit_puf_d <= 1'b0;
    end else begin
        // Default state
        rng4bit_done_reg <= 1'b0; // Clear rng4bit_done_reg flag every cycle
        response_done_2bit_puf_d <= response_done_2bit_puf; // Update the delayed version

        // Check if there is a rising edge on response_done_2bit_puf
        if (response_done_2bit_puf_rising) begin
            // Fill rng4bit_reg on successive rising edges
            if (counter < 2'b10) begin
                // Shift the response into rng4bit_reg
                rng4bit_reg <= {rng4bit_reg[1:0], response2bit_puf};
                // Increment the counter
                counter <= counter + 1;
            end
            // Check if rng4bit_reg is filled
            if (counter == 2'b01) begin // Counter was 0 before, now it's 1, so rng4bit_reg is full
                rng4bit_done_reg <= 1'b1; // Set rng4bit_done_reg high for one cycle
                counter <= 2'b00; // Reset the counter
                rng4bit_reg <= rng4bit_reg ^ challenge_rng[127:124];
            end
        end
    end
end

endmodule

