
// `include "/nfs/home/zhangdongrong/thinclient_drives/xs-env/XiangShan/src/
// main/resources/TLROT/lowrisc_dv_rot_top_verilator_sim_0.1.f"
import tlul_pkg::*;

module TLROT_top (
    input clk_i,
    input rst_ni,
    output logic ROMInitEn,
    
    output         a_ready,
    input          a_valid,
    input  [2:0]   a_bits_opcode,
    input  [2:0]   a_bits_param,
    input  [1:0]   a_bits_size,
    input  [7:0]   a_bits_source,
    input  [31:0]  a_bits_address,
    input  [3:0]   a_bits_mask,
    input  [31:0]  a_bits_data,
    input         d_ready,
    output          d_valid,
    output [2:0]   d_bits_opcode,
    output [2:0]   d_bits_param,
    output [1:0]   d_bits_size,
    output [7:0]   d_bits_source,
    output         d_bits_sink,
    output [31:0]  d_bits_data,
    output         d_bits_denied,

    output         a_ready_rom,
    input          a_valid_rom,
    input  [2:0]   a_bits_opcode_rom,
    input  [2:0]   a_bits_param_rom,
    input  [2:0]   a_bits_size_rom,
    input  [7:0]   a_bits_source_rom,
    input  [31:0]  a_bits_address_rom,
    input  [7:0]   a_bits_mask_rom,
    input  [63:0]  a_bits_data_rom,
    input         d_ready_rom,
    output          d_valid_rom,
    output [2:0]   d_bits_opcode_rom,
    output [2:0]   d_bits_param_rom,
    output [1:0]   d_bits_size_rom,
    output [7:0]   d_bits_source_rom,
    output         d_bits_sink_rom,
    output [63:0]  d_bits_data_rom,
    output         d_bits_denied_rom,

    output logic intr_hmac_hmac_done_o,
    output logic intr_hmac_fifo_empty_o,
    output logic intr_hmac_hmac_err_o,
    output logic intr_kmac_kmac_done_o,
    output logic intr_kmac_fifo_empty_o,
    output logic intr_kmac_kmac_err_o,
    output logic intr_keymgr_op_done_o,
    output logic intr_csrng_cs_cmd_req_done_o,
    output logic intr_csrng_cs_entropy_req_o,
    output logic intr_csrng_cs_hw_inst_exc_o,
    output logic intr_csrng_cs_fatal_err_o,
    output logic intr_entropy_src_es_entropy_valid_o,
    output logic intr_entropy_src_es_health_test_failed_o,
    output logic intr_entropy_src_es_observe_fifo_ready_o,
    output logic intr_entropy_src_es_fatal_err_o,
    output logic intr_edn0_edn_cmd_req_done_o,
    output logic intr_edn0_edn_fatal_err_o
);

tlul_pkg::tl_h2d_t tl_i;
tlul_pkg::tl_d2h_t tl_o;

tlul_pkg::tl_h2d_t64 rom_ctrl_rom_tl_req;
tlul_pkg::tl_d2h_t64 rom_ctrl_rom_tl_rsp;

rom_ctrl_pkg::pwrmgr_data_t       rom_ctrl_pwrmgr_data;

logic clk_edn_i;
logic rst_edn_ni;
logic rst_shadowed_ni;

// entropy src
entropy_src_pkg::entropy_src_rng_req_t       es_rng_req_o;
entropy_src_pkg::entropy_src_rng_rsp_t       es_rng_rsp_i;
logic       es_rng_fips_o;


// always_comb begin
//   if (a_bits_opcode == 3'b001 && 
//       (a_bits_mask == 4'hf || a_bits_mask == 4'h0)) begin
//     tl_i.a_opcode = 3'b000; 
//     tl_i.a_mask = 4'hf;
//   end else if (a_bits_opcode == 3'b100) begin
//     tl_i.a_opcode = 3'b100; 
//     tl_i.a_mask = 4'hf;
//   end else begin
//     tl_i.a_opcode = a_bits_opcode;
//     tl_i.a_mask = a_bits_mask; 
//   end
// end

assign tl_i.a_valid = a_valid;
assign tl_i.a_opcode = tl_a_op_e'(a_bits_opcode);  
assign tl_i.a_param = a_bits_param;
assign tl_i.a_size = a_bits_size;
assign tl_i.a_source = a_bits_source;
assign tl_i.a_address = a_bits_address;
assign tl_i.a_mask = a_bits_mask;
assign tl_i.a_data = a_bits_data;

assign tl_i.a_user = tlul_pkg::TL_A_USER_DEFAULT;
// assign tl_o.d_user = tlul_pkg:TL_D_USER_DEFAULT;

assign a_ready = tl_o.a_ready;

assign d_valid = tl_o.d_valid;
assign d_bits_opcode = tl_o.d_opcode;
assign d_bits_param = tl_o.d_param;
assign d_bits_size = tl_o.d_size;  
assign d_bits_source = tl_o.d_source;
assign d_bits_sink  = tl_o.d_sink;
assign d_bits_data = tl_o.d_data;
assign d_bits_denied = tl_o.d_error;

assign tl_i.d_ready = d_ready;

assign rom_ctrl_rom_tl_req.a_valid = a_valid_rom;
assign rom_ctrl_rom_tl_req.a_opcode = tl_a_op_e'(a_bits_opcode_rom);  
assign rom_ctrl_rom_tl_req.a_param = a_bits_param_rom;
assign rom_ctrl_rom_tl_req.a_size = a_bits_size_rom;
assign rom_ctrl_rom_tl_req.a_source = a_bits_source_rom;
assign rom_ctrl_rom_tl_req.a_address = a_bits_address_rom;
assign rom_ctrl_rom_tl_req.a_mask = a_bits_mask_rom;
assign rom_ctrl_rom_tl_req.a_data = a_bits_data_rom;

assign rom_ctrl_rom_tl_req.a_user = tlul_pkg::TL_A_USER_DEFAULT;
// assign tl_o.d_user = tlul_pkg:TL_D_USER_DEFAULT;

assign a_ready_rom = rom_ctrl_rom_tl_rsp.a_ready;

assign d_valid_rom = rom_ctrl_rom_tl_rsp.d_valid;
assign d_bits_opcode_rom = rom_ctrl_rom_tl_rsp.d_opcode;
assign d_bits_param_rom = rom_ctrl_rom_tl_rsp.d_param;
assign d_bits_size_rom = rom_ctrl_rom_tl_rsp.d_size;  
assign d_bits_source_rom = rom_ctrl_rom_tl_rsp.d_source;
assign d_bits_sink_rom  = rom_ctrl_rom_tl_rsp.d_sink;
assign d_bits_data_rom = rom_ctrl_rom_tl_rsp.d_data;
assign d_bits_denied_rom = rom_ctrl_rom_tl_rsp.d_error;

assign rom_ctrl_rom_tl_req.d_ready = d_ready_rom;

assign ROMInitEn = (rom_ctrl_pwrmgr_data==8'h66)? 1'd1 : 1'd0;

//rst_ni reverse reset!
rot_top u_rot_top (
    .clk_i(clk_i),
    .rst_ni(~rst_ni),
    .rst_shadowed_ni(~rst_ni),
    .clk_edn_i(clk_i),
    .rst_edn_ni(~rst_ni),

    .tl_i(tl_i),
    .tl_o(tl_o),

    .rom_ctrl_pwrmgr_data(rom_ctrl_pwrmgr_data),
    .rom_ctrl_rom_tl_req(rom_ctrl_rom_tl_req),
    .rom_ctrl_rom_tl_rsp(rom_ctrl_rom_tl_rsp),

    .es_rng_req_o(es_rng_req_o),
    .es_rng_rsp_i(es_rng_rsp_i),
    .es_rng_fips_o(es_rng_fips_o),

    .intr_hmac_hmac_done_o(intr_hmac_hmac_done_o),
    .intr_hmac_fifo_empty_o(intr_hmac_fifo_empty_o),  
    .intr_hmac_hmac_err_o(intr_hmac_hmac_err_o),
    .intr_kmac_kmac_done_o(intr_kmac_kmac_done_o),
    .intr_kmac_fifo_empty_o(intr_kmac_fifo_empty_o),
    .intr_kmac_kmac_err_o(intr_kmac_kmac_err_o),
    .intr_keymgr_op_done_o(intr_keymgr_op_done_o),
    .intr_csrng_cs_cmd_req_done_o(intr_csrng_cs_cmd_req_done_o),
    .intr_csrng_cs_entropy_req_o(intr_csrng_cs_entropy_req_o),  
    .intr_csrng_cs_hw_inst_exc_o(intr_csrng_cs_hw_inst_exc_o),
    .intr_csrng_cs_fatal_err_o(intr_csrng_cs_fatal_err_o),
    .intr_entropy_src_es_entropy_valid_o(intr_entropy_src_es_entropy_valid_o),
    .intr_entropy_src_es_health_test_failed_o(intr_entropy_src_es_health_test_failed_o),  
    .intr_entropy_src_es_observe_fifo_ready_o(intr_entropy_src_es_observe_fifo_ready_o),
    .intr_entropy_src_es_fatal_err_o(intr_entropy_src_es_fatal_err_o),
    .intr_edn0_edn_cmd_req_done_o(intr_edn0_edn_cmd_req_done_o),
    .intr_edn0_edn_fatal_err_o(intr_edn0_edn_fatal_err_o)
);

localparam int unsigned EntropyStreams = 4;
rng #(
  .EntropyStreams ( EntropyStreams )
) u_rng (
  .clk_i ( clk_i ),
  .rst_ni ( ~rst_ni ),
  .clk_ast_rng_i ( clk_i ),
  .rst_ast_rng_ni (  ~rst_ni ),
  .rng_en_i ( es_rng_req_o.rng_enable ),
  .rng_fips_i ( es_rng_fips_o ),
  .scan_mode_i ( 1'b0 ),
  .rng_b_o ( es_rng_rsp_i.rng_b  ),
  .rng_val_o ( es_rng_rsp_i.rng_valid )
);
    
endmodule