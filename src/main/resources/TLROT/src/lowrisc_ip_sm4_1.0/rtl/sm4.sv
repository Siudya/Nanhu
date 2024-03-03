`include "prim_assert.sv"

module sm4
  import sm4_reg_pkg::*;
(
  input  logic                                      clk_i,
  input  logic                                      rst_ni,
  // Bus interface
  input  tlul_pkg::tl_h2d_t                         tl_i,
  output tlul_pkg::tl_d2h_t                         tl_o
);

  sm4_reg2hw_t               reg2hw;
  sm4_hw2reg_t               hw2reg;
  //wire                       ready_out;

sm4_reg_top  u_sm4_reg_top (
    .clk_i                             ( clk_i        ),
    .rst_ni                            ( rst_ni       ),
    .tl_i                              ( tl_i         ),
    .hw2reg                            ( hw2reg       ),
    .devmode_i                         ( 1'b1         ),

    .tl_o                              ( tl_o         ),
    .reg2hw                            ( reg2hw       ),
    .intg_err_o                        (   )
);

/*assign hw2reg.result_out[0].de={32(ready_out)};
assign hw2reg.result_out[1].de={32(ready_out)};
assign hw2reg.result_out[2].de={32(ready_out)};
assign hw2reg.result_out[3].de={32(ready_out)};
*/
sm4_top  u_sm4_top (
    .clk                     ( clk_i        ),
    .reset_n                 ( rst_ni       ),
    .sm4_enable_in           ( reg2hw.ctrl_signals.sm4_enable_in.q),
    .encdec_enable_in        ( reg2hw.ctrl_signals.encdec_enable_in.q ),
    .encdec_sel_in           ( reg2hw.ctrl_signals.encdec_sel_in.q ),
    .valid_in                ( reg2hw.ctrl_signals.valid_in.q ),
    .data_in                 ( {reg2hw.data_in[3].q,reg2hw.data_in[2].q,reg2hw.data_in[1].q,reg2hw.data_in[0].q}),
    .enable_key_exp_in       ( reg2hw.ctrl_signals.enable_key_exp_in.q  ),
    .user_key_valid_in       ( reg2hw.ctrl_signals.user_key_valid_in.q ),
    .user_key_in             ( {reg2hw.key[3].q,reg2hw.key[2].q,reg2hw.key[1].q,reg2hw.key[0].q} ),

    //.ready_out               ( ready_out ),
    .result_out              ( {hw2reg.result_out[3].d,hw2reg.result_out[2].d,hw2reg.result_out[1].d,hw2reg.result_out[0].d} ),
    .valid_out               ( hw2reg.state_signals.valid_out.d),
    .key_exp_ready_out       ( hw2reg.state_signals.key_exp_ready_out.d),
    .ready_out_de            ( {hw2reg.result_out[3].de,hw2reg.result_out[2].de,hw2reg.result_out[1].de,hw2reg.result_out[0].de }),
    .valid_out_de            (hw2reg.state_signals.valid_out.de)
);
    
endmodule