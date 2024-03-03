module otbn_otp_controller (
  input  logic               clk,    // Clock input
  input  logic               rst_n,  // Active low reset
  input otp_ctrl_pkg::otbn_otp_key_req_t       req,
  output otp_ctrl_pkg::otbn_otp_key_rsp_t       rsp
);

// LFSR signals
//reg [127:0] lfsr_key;
//reg [63:0]  lfsr_nonce;

// Use a simple LFSR polynomial, e.g., x^128 + x^7 + x^2 + x + 1 for key
// and x^64 + x^4 + x^3 + x + 1 for nonce.
// These are not necessarily maximal length polynomials.
//wire lfsr_key_feedback = lfsr_key[127] ^ lfsr_key[7] ^ lfsr_key[2] ^ lfsr_key[1];
//wire lfsr_nonce_feedback = lfsr_nonce[63] ^ lfsr_nonce[4] ^ lfsr_nonce[3] ^ lfsr_nonce[1];

// Ack generation logic
reg ack_pulse;

// Generate a single-cycle pulse for ack
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        ack_pulse      <= 1'b0;
  end else begin
        ack_pulse <= req.req && !ack_pulse; // Generate pulse on rising edge of req
  end
end

//assign rsp.key = lfsr_key;
//assign rsp.nonce = lfsr_nonce;

// LFSR and output logic
always_ff @(posedge clk or negedge rst_n) begin
  if (!rst_n) begin
    // Reset condition
    rsp.ack        <= 1'b0;
//    rsp.key        <= {128{1'b0}}; // Initialize with a non-zero random value
//    rsp.nonce      <= {64{1'b0}};  // Initialize with a non-zero random value
    rsp.seed_valid <= 1'b1;
    rsp.key       <= 128'h4235171482c225f79289b32181a0163a; // Initialize LFSR with a non-zero value
    rsp.nonce     <= 64'h760355d3447063d1;  // Initialize LFSR with a non-zero value
  //end else if (req.req)  begin
    // Update LFSRs with feedback
  //  lfsr_key   <= {lfsr_key[126:0], lfsr_key_feedback};
   // lfsr_nonce <= {lfsr_nonce[62:0], lfsr_nonce_feedback};
  end else if (ack_pulse) begin
      // Update key and nonce with new random values on ack pulse
    //lfsr_key   <= {lfsr_key[126:0], lfsr_key_feedback};
    //lfsr_nonce <= {lfsr_nonce[62:0], lfsr_nonce_feedback};
      rsp.key <= rsp.key + 1'b1;
      rsp.nonce <= rsp.nonce + 1'b1;
      rsp.ack <= ack_pulse;
  end else begin
    // Update the ack signal with the pulse
    rsp.ack <= ack_pulse;
  end
end

endmodule
