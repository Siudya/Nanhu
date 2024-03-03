
module tb_rot_top(
    input clk_i,
    input rst_ni,
    input rst_shadowed_ni,
    input clk_edn_i,
    input rst_edn_ni 
);

    // Declare inputs and outputs
    tlul_pkg::tl_h2d_t tl_host_i;
    tlul_pkg::tl_d2h_t tl_host_o;
    keymgr_pkg::hw_key_req_t keymgr_aes_key;
    keymgr_pkg::hw_key_req_t keymgr_kmac_key;
    keymgr_pkg::otbn_key_req_t keymgr_otbn_key;
    entropy_src_pkg::entropy_src_rng_req_t es_rng_req_o;
    logic es_rng_fips_o;

    // Instantiate the DUT
    rot_top dut (
        .clk_i(clk_i),
        .rst_ni(rst_ni),
        .rst_shadowed_ni(rst_shadowed_ni),
        .clk_edn_i(clk_edn_i),
        .rst_edn_ni(rst_edn_ni),
        .tl_i(tl_host_i),
        .tl_o(tl_host_o),
        .keymgr_aes_key(keymgr_aes_key),
        // .keymgr_kmac_key(keymgr_kmac_key),
        // .keymgr_otbn_key(keymgr_otbn_key),
        .es_rng_req_o(es_rng_req_o),
        .es_rng_fips_o(es_rng_fips_o)
    );

logic [11:0] cnt;

 always_ff @( posedge clk_i , negedge rst_ni ) begin
  if (!rst_ni) begin
    cnt <= 12'd0;
  end else begin
    if (cnt == 2000) begin
      cnt <= cnt;
    end else begin
      cnt <= cnt + 1'd1;
    end
  end
 end
  
 always_ff @( posedge clk_i , negedge rst_ni ) begin
  if (!rst_ni) begin
    tl_host_i <= tlul_pkg::TL_H2D_DEFAULT;
  end else begin
    if (cnt == 1500) begin
      tl_host_i.a_valid <= 1'd1;
      tl_host_i.a_opcode <= 3'h 4;
      tl_host_i.a_address <= tl_main_rot_pkg::ADDR_SPACE_KEYMGR_ROT + keymgr_reg_pkg::KEYMGR_CONTROL_SHADOWED_OFFSET; 
      tl_host_i.a_data <= 32'h 12;
      tl_host_i.a_size <= 2;
      tl_host_i.a_user <= tlul_pkg::TL_A_USER_DEFAULT;
      tl_host_i.d_ready <= 1'd1;
    
    end else if (cnt == 1505) begin
      tl_host_i <= tlul_pkg::TL_H2D_DEFAULT;
    end
  end
 end


endmodule

