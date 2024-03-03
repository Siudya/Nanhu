`include "prim_assert.sv"


module sm3
  import sm3_reg_pkg::*;
(
  input  logic                                      clk_i,
  input  logic                                      rst_ni,
  // Bus interface
  input  tlul_pkg::tl_h2d_t                         tl_i,
  output tlul_pkg::tl_d2h_t                         tl_o
);

  sm3_reg2hw_t               reg2hw;
  sm3_hw2reg_t               hw2reg;
  //wire                       ready_out;

  sm3_reg_top  u_sm3_reg_top (
    .clk_i                             ( clk_i           ),
    .rst_ni                            ( rst_ni          ),
    .tl_i                              ( tl_i            ),
    .hw2reg                            ( hw2reg          ),
    .devmode_i                         ( 1'b1            ),

    .tl_o                              ( tl_o            ),
    .reg2hw                            ( reg2hw          ),
    .intg_err_o                        (                 )
);

sm3_core_top  u_sm3_core_top (
    .clk                     ( clk_i  ),
    .rst_n                   ( rst_ni ),
    .msg_inpt_d              ( reg2hw.message_in.q ),
    .msg_inpt_vld_byte       ( {reg2hw.ctrl_signals.msg_inpt_vld_byte_3.q,reg2hw.ctrl_signals.msg_inpt_vld_byte_2.q,reg2hw.ctrl_signals.msg_inpt_vld_byte_1.q,reg2hw.ctrl_signals.msg_inpt_vld_byte_0.q} ),
    .msg_inpt_vld            ( reg2hw.message_in.qe ),
    .msg_inpt_lst            ( reg2hw.ctrl_signals.msg_inpt_lst.q ),

    .msg_inpt_rdy            ( hw2reg.state_signals.msg_inpt_rdy.d ),
    .cmprss_otpt_res         ( {hw2reg.result_out[7].d,hw2reg.result_out[6].d,hw2reg.result_out[5].d,hw2reg.result_out[4].d,hw2reg.result_out[3].d,hw2reg.result_out[2].d,hw2reg.result_out[1].d,hw2reg.result_out[0].d} ),
    .cmprss_otpt_vld         ( hw2reg.state_signals.cmprss_otpt_vld.d ),
    .msg_inpt_rdy_re         ( hw2reg.state_signals.msg_inpt_rdy.de ),
    .cmprss_vld_re           ( hw2reg.state_signals.cmprss_otpt_vld.de ),
    .cmprss_res_re           ( {hw2reg.result_out[7].de,hw2reg.result_out[6].de,hw2reg.result_out[5].de,hw2reg.result_out[4].de,hw2reg.result_out[3].de,hw2reg.result_out[2].de,hw2reg.result_out[1].de,hw2reg.result_out[0].de} )
);


endmodule
