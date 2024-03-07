// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0
//
// Register Package auto-generated by `reggen` containing data structure

package sm3_reg_pkg;

  // Param list
  parameter int ResultData = 8;

  // Address widths within the block
  parameter int BlockAw = 6;

  ////////////////////////////
  // Typedefs for registers //
  ////////////////////////////

  typedef struct packed {
    struct packed {
      logic        q;
      logic        qe;
    } msg_inpt_lst;
    struct packed {
      logic        q;
      logic        qe;
    } msg_inpt_vld_byte_0;
    struct packed {
      logic        q;
      logic        qe;
    } msg_inpt_vld_byte_1;
    struct packed {
      logic        q;
      logic        qe;
    } msg_inpt_vld_byte_2;
    struct packed {
      logic        q;
      logic        qe;
    } msg_inpt_vld_byte_3;
  } sm3_reg2hw_ctrl_signals_reg_t;

  typedef struct packed {
    struct packed {
      logic        q;
    } cmprss_otpt_vld;
    struct packed {
      logic        q;
    } msg_inpt_rdy;
  } sm3_reg2hw_state_signals_reg_t;

  typedef struct packed {
    logic [31:0] q;
    logic        qe;
  } sm3_reg2hw_message_in_reg_t;

  typedef struct packed {
    logic [31:0] q;
  } sm3_reg2hw_result_out_mreg_t;

  typedef struct packed {
    struct packed {
      logic        d;
      logic        de;
    } msg_inpt_lst;
    struct packed {
      logic        d;
      logic        de;
    } msg_inpt_vld_byte_0;
    struct packed {
      logic        d;
      logic        de;
    } msg_inpt_vld_byte_1;
    struct packed {
      logic        d;
      logic        de;
    } msg_inpt_vld_byte_2;
    struct packed {
      logic        d;
      logic        de;
    } msg_inpt_vld_byte_3;
  } sm3_hw2reg_ctrl_signals_reg_t;

  typedef struct packed {
    struct packed {
      logic        d;
      logic        de;
    } cmprss_otpt_vld;
    struct packed {
      logic        d;
      logic        de;
    } msg_inpt_rdy;
  } sm3_hw2reg_state_signals_reg_t;

  typedef struct packed {
    logic [31:0] d;
    logic        de;
  } sm3_hw2reg_message_in_reg_t;

  typedef struct packed {
    logic [31:0] d;
    logic        de;
  } sm3_hw2reg_result_out_mreg_t;

  // Register -> HW type
  typedef struct packed {
    sm3_reg2hw_ctrl_signals_reg_t ctrl_signals; // [300:291]
    sm3_reg2hw_state_signals_reg_t state_signals; // [290:289]
    sm3_reg2hw_message_in_reg_t message_in; // [288:256]
    sm3_reg2hw_result_out_mreg_t [7:0] result_out; // [255:0]
  } sm3_reg2hw_t;

  // HW -> register type
  typedef struct packed {
    sm3_hw2reg_ctrl_signals_reg_t ctrl_signals; // [310:301]
    sm3_hw2reg_state_signals_reg_t state_signals; // [300:297]
    sm3_hw2reg_message_in_reg_t message_in; // [296:264]
    sm3_hw2reg_result_out_mreg_t [7:0] result_out; // [263:0]
  } sm3_hw2reg_t;

  // Register offsets
  parameter logic [BlockAw-1:0] SM3_CTRL_SIGNALS_OFFSET = 6'h 0;
  parameter logic [BlockAw-1:0] SM3_STATE_SIGNALS_OFFSET = 6'h 4;
  parameter logic [BlockAw-1:0] SM3_MESSAGE_IN_OFFSET = 6'h 8;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_0_OFFSET = 6'h c;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_1_OFFSET = 6'h 10;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_2_OFFSET = 6'h 14;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_3_OFFSET = 6'h 18;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_4_OFFSET = 6'h 1c;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_5_OFFSET = 6'h 20;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_6_OFFSET = 6'h 24;
  parameter logic [BlockAw-1:0] SM3_RESULT_OUT_7_OFFSET = 6'h 28;

  // Register index
  typedef enum int {
    SM3_CTRL_SIGNALS,
    SM3_STATE_SIGNALS,
    SM3_MESSAGE_IN,
    SM3_RESULT_OUT_0,
    SM3_RESULT_OUT_1,
    SM3_RESULT_OUT_2,
    SM3_RESULT_OUT_3,
    SM3_RESULT_OUT_4,
    SM3_RESULT_OUT_5,
    SM3_RESULT_OUT_6,
    SM3_RESULT_OUT_7
  } sm3_id_e;

  // Register width information to check illegal writes
  parameter logic [3:0] SM3_PERMIT [11] = '{
    4'b 0001, // index[ 0] SM3_CTRL_SIGNALS
    4'b 0001, // index[ 1] SM3_STATE_SIGNALS
    4'b 1111, // index[ 2] SM3_MESSAGE_IN
    4'b 1111, // index[ 3] SM3_RESULT_OUT_0
    4'b 1111, // index[ 4] SM3_RESULT_OUT_1
    4'b 1111, // index[ 5] SM3_RESULT_OUT_2
    4'b 1111, // index[ 6] SM3_RESULT_OUT_3
    4'b 1111, // index[ 7] SM3_RESULT_OUT_4
    4'b 1111, // index[ 8] SM3_RESULT_OUT_5
    4'b 1111, // index[ 9] SM3_RESULT_OUT_6
    4'b 1111  // index[10] SM3_RESULT_OUT_7
  };

endpackage