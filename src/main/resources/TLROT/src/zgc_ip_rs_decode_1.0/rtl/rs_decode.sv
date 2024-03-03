`include "prim_assert.sv"

module rs_decode
  import rs_decode_reg_pkg::*;
(
  input  logic                                      clk_i,
  input  logic                                      rst_ni,
  // Bus interface
  input  tlul_pkg::tl_h2d_t                         tl_i,
  output tlul_pkg::tl_d2h_t                         tl_o
);

  rs_decode_reg2hw_t               reg2hw;
  rs_decode_hw2reg_t               hw2reg;
  //wire                       ready_out;

  rs_decode_reg_top  u_rs_decode_reg_top (
    .clk_i                             ( clk_i           ),
    .rst_ni                            ( rst_ni          ),
    .tl_i                              ( tl_i            ),
    .hw2reg                            ( hw2reg          ),
    .devmode_i                         ( 1'b1            ),

    .tl_o                              ( tl_o            ),
    .reg2hw                            ( reg2hw          ),
    .intg_err_o                        (                 )
);

  rs_decode_wrapper  u_rs_decode_wrapper (
    .clk                     ( clk_i        ),
    .rst_n                   ( rst_ni       ),
    .decode_en               ( reg2hw.ctrl_signals.decode_en.q),
    .clrn                    ( reg2hw.ctrl_signals.clrn.q ),
    .encoded_data            ( {reg2hw.encoded_data_in[49].q,reg2hw.encoded_data_in[48].q,reg2hw.encoded_data_in[47].q,reg2hw.encoded_data_in[46].q,reg2hw.encoded_data_in[45].q,reg2hw.encoded_data_in[44].q,reg2hw.encoded_data_in[43].q,reg2hw.encoded_data_in[42].q,reg2hw.encoded_data_in[41].q,reg2hw.encoded_data_in[40].q,reg2hw.encoded_data_in[39].q,reg2hw.encoded_data_in[38].q,reg2hw.encoded_data_in[37].q,reg2hw.encoded_data_in[36].q,reg2hw.encoded_data_in[35].q,reg2hw.encoded_data_in[34].q,reg2hw.encoded_data_in[33].q,reg2hw.encoded_data_in[32].q,reg2hw.encoded_data_in[31].q,reg2hw.encoded_data_in[30].q,reg2hw.encoded_data_in[29].q,reg2hw.encoded_data_in[28].q,reg2hw.encoded_data_in[27].q,reg2hw.encoded_data_in[26].q,reg2hw.encoded_data_in[25].q,reg2hw.encoded_data_in[24].q,reg2hw.encoded_data_in[23].q,reg2hw.encoded_data_in[22].q,reg2hw.encoded_data_in[21].q,reg2hw.encoded_data_in[20].q,reg2hw.encoded_data_in[19].q,reg2hw.encoded_data_in[18].q,reg2hw.encoded_data_in[17].q,reg2hw.encoded_data_in[16].q,reg2hw.encoded_data_in[15].q,reg2hw.encoded_data_in[14].q,reg2hw.encoded_data_in[13].q,reg2hw.encoded_data_in[12].q,reg2hw.encoded_data_in[11].q,reg2hw.encoded_data_in[10].q,reg2hw.encoded_data_in[9].q,reg2hw.encoded_data_in[8].q,reg2hw.encoded_data_in[7].q,reg2hw.encoded_data_in[6].q,reg2hw.encoded_data_in[5].q,reg2hw.encoded_data_in[4].q,reg2hw.encoded_data_in[3].q,reg2hw.encoded_data_in[2].q,reg2hw.encoded_data_in[1].q,reg2hw.encoded_data_in[0].q} ),

    .error_pos               ( {hw2reg.error_pos_out[49].d,hw2reg.error_pos_out[48].d,hw2reg.error_pos_out[47].d,hw2reg.error_pos_out[46].d,hw2reg.error_pos_out[45].d,hw2reg.error_pos_out[44].d,hw2reg.error_pos_out[43].d,hw2reg.error_pos_out[42].d,hw2reg.error_pos_out[41].d,hw2reg.error_pos_out[40].d,hw2reg.error_pos_out[39].d,hw2reg.error_pos_out[38].d,hw2reg.error_pos_out[37].d,hw2reg.error_pos_out[36].d,hw2reg.error_pos_out[35].d,hw2reg.error_pos_out[34].d,hw2reg.error_pos_out[33].d,hw2reg.error_pos_out[32].d,hw2reg.error_pos_out[31].d,hw2reg.error_pos_out[30].d,hw2reg.error_pos_out[29].d,hw2reg.error_pos_out[28].d,hw2reg.error_pos_out[27].d,hw2reg.error_pos_out[26].d,hw2reg.error_pos_out[25].d,hw2reg.error_pos_out[24].d,hw2reg.error_pos_out[23].d,hw2reg.error_pos_out[22].d,hw2reg.error_pos_out[21].d,hw2reg.error_pos_out[20].d,hw2reg.error_pos_out[19].d,hw2reg.error_pos_out[18].d,hw2reg.error_pos_out[17].d,hw2reg.error_pos_out[16].d,hw2reg.error_pos_out[15].d,hw2reg.error_pos_out[14].d,hw2reg.error_pos_out[13].d,hw2reg.error_pos_out[12].d,hw2reg.error_pos_out[11].d,hw2reg.error_pos_out[10].d,hw2reg.error_pos_out[9].d,hw2reg.error_pos_out[8].d,hw2reg.error_pos_out[7].d,hw2reg.error_pos_out[6].d,hw2reg.error_pos_out[5].d,hw2reg.error_pos_out[4].d,hw2reg.error_pos_out[3].d,hw2reg.error_pos_out[2].d,hw2reg.error_pos_out[1].d,hw2reg.error_pos_out[0].d} ),
    .output_valid            ( hw2reg.state_signals.output_valid_bit.d ),
    .ready                   ( hw2reg.state_signals.ready_bit.d ),
    .with_error              ( hw2reg.state_signals.with_error_bit.d ),
    .ready_re                ( hw2reg.state_signals.ready_bit.de ),
    .output_valid_re         ( hw2reg.state_signals.output_valid_bit.de ),
    .error_pos_re            ( {hw2reg.error_pos_out[49].de,hw2reg.error_pos_out[48].de,hw2reg.error_pos_out[47].de,hw2reg.error_pos_out[46].de,hw2reg.error_pos_out[45].de,hw2reg.error_pos_out[44].de,hw2reg.error_pos_out[43].de,hw2reg.error_pos_out[42].de,hw2reg.error_pos_out[41].de,hw2reg.error_pos_out[40].de,hw2reg.error_pos_out[39].de,hw2reg.error_pos_out[38].de,hw2reg.error_pos_out[37].de,hw2reg.error_pos_out[36].de,hw2reg.error_pos_out[35].de,hw2reg.error_pos_out[34].de,hw2reg.error_pos_out[33].de,hw2reg.error_pos_out[32].de,hw2reg.error_pos_out[31].de,hw2reg.error_pos_out[30].de,hw2reg.error_pos_out[29].de,hw2reg.error_pos_out[28].de,hw2reg.error_pos_out[27].de,hw2reg.error_pos_out[26].de,hw2reg.error_pos_out[25].de,hw2reg.error_pos_out[24].de,hw2reg.error_pos_out[23].de,hw2reg.error_pos_out[22].de,hw2reg.error_pos_out[21].de,hw2reg.error_pos_out[20].de,hw2reg.error_pos_out[19].de,hw2reg.error_pos_out[18].de,hw2reg.error_pos_out[17].de,hw2reg.error_pos_out[16].de,hw2reg.error_pos_out[15].de,hw2reg.error_pos_out[14].de,hw2reg.error_pos_out[13].de,hw2reg.error_pos_out[12].de,hw2reg.error_pos_out[11].de,hw2reg.error_pos_out[10].de,hw2reg.error_pos_out[9].de,hw2reg.error_pos_out[8].de,hw2reg.error_pos_out[7].de,hw2reg.error_pos_out[6].de,hw2reg.error_pos_out[5].de,hw2reg.error_pos_out[4].de,hw2reg.error_pos_out[3].de,hw2reg.error_pos_out[2].de,hw2reg.error_pos_out[1].de,hw2reg.error_pos_out[0].de} ),
    .with_error_re           ( hw2reg.state_signals.with_error_bit.de )
); 

endmodule


