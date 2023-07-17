
import "DPI-C" function void L2MP_write
(
  input longint metaWway,
  input longint metaWvalid,
  input longint mshrId,
  input longint allocPtr,
  input longint allocValid,
  input longint dirWay,
  input longint dirHit,
  input longint sset,
  input longint tag,
  input longint opcode,
  input longint channel,
  input longint mshrTask,
  input longint stamp,
  input string site
);

module L2MPWriter(
  input clock,
  input reset,
  input en,
  input [2:0] data_metaWway,
  input [0:0] data_metaWvalid,
  input [7:0] data_mshrId,
  input [7:0] data_allocPtr,
  input [0:0] data_allocValid,
  input [2:0] data_dirWay,
  input [0:0] data_dirHit,
  input [8:0] data_sset,
  input [18:0] data_tag,
  input [2:0] data_opcode,
  input [2:0] data_channel,
  input [0:0] data_mshrTask,
  input [63:0] stamp
);
  parameter string site;

  always@(posedge clock) begin
    if(en && !reset) begin
      L2MP_write(data_metaWway, data_metaWvalid, data_mshrId, data_allocPtr, data_allocValid, data_dirWay, data_dirHit, data_sset, data_tag, data_opcode, data_channel, data_mshrTask, stamp, site);
    end
  end
endmodule
