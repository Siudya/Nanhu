// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// This file is auto-generated.
// Used parser: Fallback (regex)


// This is to prevent AscentLint warnings in the generated
// abstract prim wrapper. These warnings occur due to the .*
// use. TODO: we may want to move these inline waivers
// into a separate, generated waiver file for consistency.
//ri lint_check_off OUTPUT_NOT_DRIVEN INPUT_NOT_READ HIER_BRANCH_NOT_READ
module prim_pad_wrapper
import prim_pad_wrapper_pkg::*;
#(

  // These parameters are ignored in this model.
  parameter pad_type_e PadType = BidirStd,
  parameter scan_role_e ScanRole = NoScan

) (
  // This is only used for scanmode (not used in generic models)
  input              clk_scan_i,
  input              scanmode_i,
  // Power sequencing signals (not used in generic models)
  input pad_pok_t    pok_i,
  // Main Pad signals
  inout wire         inout_io, // bidirectional pad
  output logic       in_o,     // input data
  output logic       in_raw_o, // uninverted output data
  input              ie_i,     // input enable
  input              out_i,    // output data
  input              oe_i,     // output enable
  input pad_attr_t   attr_i    // additional pad attributes
);

  if (1) begin : gen_generic
    prim_generic_pad_wrapper #(
      .PadType(PadType),
      .ScanRole(ScanRole)
    ) u_impl_generic (
      .*
    );

  end

endmodule
//ri lint_check_on OUTPUT_NOT_DRIVEN INPUT_NOT_READ HIER_BRANCH_NOT_READ
