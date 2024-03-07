`include "prim_assert.sv"

module puf
  import puf_reg_pkg::*;
(
  input  logic                                      clk_i,
  input  logic                                      rst_ni,

  output [3:0] rng4bit,
  output rng4bit_done,
  output rng_mode,
  input es_rng_req,

  // Bus interface
  input  tlul_pkg::tl_h2d_t                         tl_i,
  output tlul_pkg::tl_d2h_t                         tl_o
);

  puf_reg2hw_t               reg2hw;
  puf_hw2reg_t               hw2reg;
  //wire                       ready_out;

puf_reg_top  u_puf_reg_top (
    .clk_i                             ( clk_i  ),
    .rst_ni                            ( rst_ni ),
    .tl_i                              ( tl_i   ),
    .hw2reg                            ( hw2reg ),
    .devmode_i                         ( 1'b1   ),

    .tl_o                              ( tl_o   ),
    .reg2hw                            ( reg2hw ),
    .intg_err_o                        ( )
);


PUF_core  u_PUF_core (
    .clk                     ( clk_i  ),
    .rst_n                   ( rst_ni ),
    .enable                  ( reg2hw.ctrl_signals.enable_puf.q ),
    .mode                    ( reg2hw.ctrl_signals.mode_puf.q ),
    .ready_challenge         ( reg2hw.ctrl_signals.ready_cha.q ),
    .challenge               ( {reg2hw.challenge[3].q,reg2hw.challenge[2].q,reg2hw.challenge[1].q,reg2hw.challenge[0].q} ),

    .response_done           (  ),//暂时无用
    .response_valid          ( hw2reg.state_signals.response_valid_bit.d ),
    .response                ( {hw2reg.response[7].d,hw2reg.response[6].d,hw2reg.response[5].d,hw2reg.response[4].d,hw2reg.response[3].d,hw2reg.response[2].d,hw2reg.response[1].d,hw2reg.response[0].d} ),
    .response_valid_re       ( hw2reg.state_signals.response_valid_bit.de ),
    .response_re             ( {hw2reg.response[7].de,hw2reg.response[6].de,hw2reg.response[5].de,hw2reg.response[4].de,hw2reg.response[3].de,hw2reg.response[2].de,hw2reg.response[1].de,hw2reg.response[0].de} ),
    .response_done_2bit_re   ( hw2reg.state_signals.response_done_2bit.de ),
    .response_done_2bit      ( hw2reg.state_signals.response_done_2bit.d ),
    .response2bit            (  ),//暂时无用
    .rng4bit                 ( rng4bit ),
    .rng4bit_done            ( rng4bit_done ),
    .rng_mode                ( rng_mode ),
    .es_rng_req              ( es_rng_req )
);

endmodule

