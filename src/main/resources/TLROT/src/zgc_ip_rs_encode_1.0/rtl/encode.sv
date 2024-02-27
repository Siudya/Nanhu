// -------------------------------------------------------------------------
//Reed-Solomon Encoder
//Copyright (C) Tue Apr  2 17:06:57 2002
//by Ming-Han Lei(hendrik@humanistic.org)
//
//This program is free software; you can redistribute it and/or
//modify it under the terms of the GNU Lesser General Public License
//as published by the Free Software Foundation; either version 2
//of the License, or (at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU Lesser General Public License for more details.
//
//You should have received a copy of the GNU Lesser General Public License
//along with this program; if not, write to the Free Software
//Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
// --------------------------------------------------------------------------

module rs_enc_m0 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[3] ^ x[5] ^ x[6];
		y[1] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[3] ^ x[4];
		y[3] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[4] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[2] ^ x[4] ^ x[5] ^ x[7];
	end
endmodule

module rs_enc_m1 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[2] ^ x[4] ^ x[5];
		y[1] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[6];
		y[2] = x[0] ^ x[7];
		y[3] = x[1];
		y[4] = x[0] ^ x[2];
		y[5] = x[0] ^ x[1] ^ x[3];
		y[6] = x[1] ^ x[2] ^ x[4];
		y[7] = x[0] ^ x[1] ^ x[3] ^ x[4];
	end
endmodule

module rs_enc_m2 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[1] = x[0] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[3] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[4] = x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[3] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[4] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[7];
	end
endmodule

module rs_enc_m3 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[2] ^ x[4] ^ x[5] ^ x[7];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[3];
		y[3] = x[1] ^ x[2] ^ x[4];
		y[4] = x[0] ^ x[2] ^ x[3] ^ x[5];
		y[5] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[6];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[7];
		y[7] = x[1] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
	end
endmodule

module rs_enc_m4 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[5] ^ x[7];
		y[1] = x[2] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[3] ^ x[5] ^ x[6];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[6] ^ x[7];
		y[4] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[7];
		y[5] = x[2] ^ x[3] ^ x[4] ^ x[6];
		y[6] = x[0] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[7] = x[0] ^ x[4] ^ x[6] ^ x[7];
	end
endmodule

module rs_enc_m5 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[4];
		y[1] = x[0] ^ x[2] ^ x[4] ^ x[5];
		y[2] = x[3] ^ x[4] ^ x[5] ^ x[6];
		y[3] = x[0] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[1] ^ x[2] ^ x[6] ^ x[7];
		y[6] = x[0] ^ x[2] ^ x[3] ^ x[7];
		y[7] = x[0] ^ x[3];
	end
endmodule

module rs_enc_m6 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[3] ^ x[4];
		y[2] = x[2] ^ x[6] ^ x[7];
		y[3] = x[3] ^ x[7];
		y[4] = x[0] ^ x[4];
		y[5] = x[0] ^ x[1] ^ x[5];
		y[6] = x[1] ^ x[2] ^ x[6];
		y[7] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
	end
endmodule

module rs_enc_m7 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[2] = x[4] ^ x[7];
		y[3] = x[0] ^ x[5];
		y[4] = x[0] ^ x[1] ^ x[6];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[7];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[3];
		y[7] = x[0] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
	end
endmodule

module rs_enc_m8 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[6];
		y[1] = x[2] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[3] ^ x[6] ^ x[7];
		y[3] = x[1] ^ x[2] ^ x[4] ^ x[7];
		y[4] = x[2] ^ x[3] ^ x[5];
		y[5] = x[3] ^ x[4] ^ x[6];
		y[6] = x[0] ^ x[4] ^ x[5] ^ x[7];
		y[7] = x[0] ^ x[5];
	end
endmodule

module rs_enc_m9 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[2] ^ x[5] ^ x[7];
		y[1] = x[1] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[4] ^ x[5] ^ x[6];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[1] ^ x[2] ^ x[3] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[7];
		y[6] = x[1] ^ x[3] ^ x[4] ^ x[5];
		y[7] = x[0] ^ x[1] ^ x[4] ^ x[6] ^ x[7];
	end
endmodule

module rs_enc_m10 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[3] ^ x[4] ^ x[7];
		y[1] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[2] ^ x[3] ^ x[7];
		y[4] = x[1] ^ x[3] ^ x[4];
		y[5] = x[0] ^ x[2] ^ x[4] ^ x[5];
		y[6] = x[0] ^ x[1] ^ x[3] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[2] ^ x[3] ^ x[6];
	end
endmodule

module rs_enc_m11 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[1] = x[2] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[4];
		y[3] = x[0] ^ x[1] ^ x[3] ^ x[5];
		y[4] = x[1] ^ x[2] ^ x[4] ^ x[6];
		y[5] = x[2] ^ x[3] ^ x[5] ^ x[7];
		y[6] = x[0] ^ x[3] ^ x[4] ^ x[6];
		y[7] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6];
	end
endmodule

module rs_enc_m12 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[3];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4];
		y[2] = x[2] ^ x[4] ^ x[5];
		y[3] = x[3] ^ x[5] ^ x[6];
		y[4] = x[0] ^ x[4] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[1] ^ x[5] ^ x[7];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[6];
		y[7] = x[0] ^ x[2] ^ x[7];
	end
endmodule

module rs_enc_m13 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[2] ^ x[3] ^ x[6] ^ x[7];
		y[1] = x[2] ^ x[4] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[5] ^ x[6];
		y[3] = x[1] ^ x[3] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[2] ^ x[4] ^ x[7];
		y[5] = x[1] ^ x[3] ^ x[5];
		y[6] = x[0] ^ x[2] ^ x[4] ^ x[6];
		y[7] = x[1] ^ x[2] ^ x[5] ^ x[6];
	end
endmodule

module rs_enc_m14 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[5];
		y[1] = x[1] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[3] ^ x[6] ^ x[7];
		y[4] = x[1] ^ x[2] ^ x[4] ^ x[7];
		y[5] = x[2] ^ x[3] ^ x[5];
		y[6] = x[3] ^ x[4] ^ x[6];
		y[7] = x[4] ^ x[7];
	end
endmodule

module rs_enc_m15 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[1] = x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[3] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[4] = x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[4] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6];
	end
endmodule

module rs_enc_m16 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[1];
		y[2] = x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[4] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[5] ^ x[6] ^ x[7];
		y[5] = x[6] ^ x[7];
		y[6] = x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
	end
endmodule

module rs_enc_m17 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[5];
		y[1] = x[1] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[3] ^ x[6] ^ x[7];
		y[4] = x[1] ^ x[2] ^ x[4] ^ x[7];
		y[5] = x[2] ^ x[3] ^ x[5];
		y[6] = x[3] ^ x[4] ^ x[6];
		y[7] = x[4] ^ x[7];
	end
endmodule

module rs_enc_m18 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[3] ^ x[5];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[4] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[3] ^ x[5] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[6];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[7];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6];
		y[7] = x[0] ^ x[2] ^ x[4] ^ x[7];
	end
endmodule

module rs_enc_m19 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[1] = x[3] ^ x[4] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[6];
		y[3] = x[2] ^ x[3] ^ x[7];
		y[4] = x[0] ^ x[3] ^ x[4];
		y[5] = x[0] ^ x[1] ^ x[4] ^ x[5];
		y[6] = x[1] ^ x[2] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
	end
endmodule

module rs_enc_m20 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[3] ^ x[6];
		y[1] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[2] ^ x[5] ^ x[7];
	end
endmodule

module rs_enc_m21 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[3] ^ x[5];
		y[2] = x[2] ^ x[4] ^ x[5] ^ x[7];
		y[3] = x[0] ^ x[3] ^ x[5] ^ x[6];
		y[4] = x[1] ^ x[4] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[2] ^ x[5] ^ x[7];
		y[6] = x[1] ^ x[3] ^ x[6];
		y[7] = x[0] ^ x[1] ^ x[4] ^ x[5] ^ x[6];
	end
endmodule

module rs_enc_m22 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[7];
		y[1] = x[3] ^ x[4] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2];
		y[3] = x[1] ^ x[2] ^ x[3];
		y[4] = x[0] ^ x[2] ^ x[3] ^ x[4];
		y[5] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5];
		y[6] = x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[6];
	end
endmodule

module rs_enc_m23 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6];
		y[1] = x[4] ^ x[5] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[3];
		y[3] = x[0] ^ x[2] ^ x[3] ^ x[4];
		y[4] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5];
		y[5] = x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[6] = x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[7];
	end
endmodule

module rs_enc_m24 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[1] = x[2] ^ x[3] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[5];
		y[3] = x[1] ^ x[2] ^ x[6];
		y[4] = x[2] ^ x[3] ^ x[7];
		y[5] = x[0] ^ x[3] ^ x[4];
		y[6] = x[0] ^ x[1] ^ x[4] ^ x[5];
		y[7] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
	end
endmodule

module rs_enc_m25 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[7];
		y[1] = x[3] ^ x[4] ^ x[5] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[2] ^ x[3] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[7];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5];
		y[6] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[1] ^ x[3] ^ x[6];
	end
endmodule

module rs_enc_m26 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[3] ^ x[5];
		y[1] = x[3] ^ x[4] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[4] ^ x[5] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[5] ^ x[6];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[6] ^ x[7];
		y[6] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[7];
		y[7] = x[2] ^ x[4];
	end
endmodule

module rs_enc_m27 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[3];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4];
		y[2] = x[2] ^ x[4] ^ x[5];
		y[3] = x[3] ^ x[5] ^ x[6];
		y[4] = x[0] ^ x[4] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[1] ^ x[5] ^ x[7];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[6];
		y[7] = x[0] ^ x[2] ^ x[7];
	end
endmodule

module rs_enc_m28 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[4] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[3] ^ x[5] ^ x[6];
		y[3] = x[1] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[2] ^ x[4] ^ x[5] ^ x[7];
		y[5] = x[1] ^ x[3] ^ x[5] ^ x[6];
		y[6] = x[2] ^ x[4] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[5] ^ x[6];
	end
endmodule

module rs_enc_m29 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[1] ^ x[2] ^ x[3] ^ x[5];
		y[1] = x[0] ^ x[1] ^ x[4] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[3] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[4] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[5];
		y[5] = x[1] ^ x[2] ^ x[3] ^ x[6];
		y[6] = x[2] ^ x[3] ^ x[4] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[4];
	end
endmodule

module rs_enc_m30 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[3] ^ x[5] ^ x[7];
		y[1] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[2] ^ x[4] ^ x[6];
	end
endmodule

module rs_enc_m31 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;
	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[6];
		y[1] = x[4] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[5] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[6] = x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[5] ^ x[7];
	end
endmodule

module rs_enc (y, x, enable, data, clk, clrn);
	input [7:0] x;
	input clk, clrn, enable, data;
	output [7:0] y;
	reg [7:0] y;

	wire [7:0] scale0;
	wire [7:0] scale1;
	wire [7:0] scale2;
	wire [7:0] scale3;
	wire [7:0] scale4;
	wire [7:0] scale5;
	wire [7:0] scale6;
	wire [7:0] scale7;
	wire [7:0] scale8;
	wire [7:0] scale9;
	wire [7:0] scale10;
	wire [7:0] scale11;
	wire [7:0] scale12;
	wire [7:0] scale13;
	wire [7:0] scale14;
	wire [7:0] scale15;
	wire [7:0] scale16;
	wire [7:0] scale17;
	wire [7:0] scale18;
	wire [7:0] scale19;
	wire [7:0] scale20;
	wire [7:0] scale21;
	wire [7:0] scale22;
	wire [7:0] scale23;
	wire [7:0] scale24;
	wire [7:0] scale25;
	wire [7:0] scale26;
	wire [7:0] scale27;
	wire [7:0] scale28;
	wire [7:0] scale29;
	wire [7:0] scale30;
	wire [7:0] scale31;
	reg [7:0] mem0;
	reg [7:0] mem1;
	reg [7:0] mem2;
	reg [7:0] mem3;
	reg [7:0] mem4;
	reg [7:0] mem5;
	reg [7:0] mem6;
	reg [7:0] mem7;
	reg [7:0] mem8;
	reg [7:0] mem9;
	reg [7:0] mem10;
	reg [7:0] mem11;
	reg [7:0] mem12;
	reg [7:0] mem13;
	reg [7:0] mem14;
	reg [7:0] mem15;
	reg [7:0] mem16;
	reg [7:0] mem17;
	reg [7:0] mem18;
	reg [7:0] mem19;
	reg [7:0] mem20;
	reg [7:0] mem21;
	reg [7:0] mem22;
	reg [7:0] mem23;
	reg [7:0] mem24;
	reg [7:0] mem25;
	reg [7:0] mem26;
	reg [7:0] mem27;
	reg [7:0] mem28;
	reg [7:0] mem29;
	reg [7:0] mem30;
	reg [7:0] mem31;
	reg [7:0] feedback;

	rs_enc_m0 m0 (scale0, feedback);
	rs_enc_m1 m1 (scale1, feedback);
	rs_enc_m2 m2 (scale2, feedback);
	rs_enc_m3 m3 (scale3, feedback);
	rs_enc_m4 m4 (scale4, feedback);
	rs_enc_m5 m5 (scale5, feedback);
	rs_enc_m6 m6 (scale6, feedback);
	rs_enc_m7 m7 (scale7, feedback);
	rs_enc_m8 m8 (scale8, feedback);
	rs_enc_m9 m9 (scale9, feedback);
	rs_enc_m10 m10 (scale10, feedback);
	rs_enc_m11 m11 (scale11, feedback);
	rs_enc_m12 m12 (scale12, feedback);
	rs_enc_m13 m13 (scale13, feedback);
	rs_enc_m14 m14 (scale14, feedback);
	rs_enc_m15 m15 (scale15, feedback);
	rs_enc_m16 m16 (scale16, feedback);
	rs_enc_m17 m17 (scale17, feedback);
	rs_enc_m18 m18 (scale18, feedback);
	rs_enc_m19 m19 (scale19, feedback);
	rs_enc_m20 m20 (scale20, feedback);
	rs_enc_m21 m21 (scale21, feedback);
	rs_enc_m22 m22 (scale22, feedback);
	rs_enc_m23 m23 (scale23, feedback);
	rs_enc_m24 m24 (scale24, feedback);
	rs_enc_m25 m25 (scale25, feedback);
	rs_enc_m26 m26 (scale26, feedback);
	rs_enc_m27 m27 (scale27, feedback);
	rs_enc_m28 m28 (scale28, feedback);
	rs_enc_m29 m29 (scale29, feedback);
	rs_enc_m30 m30 (scale30, feedback);
	rs_enc_m31 m31 (scale31, feedback);

	always @ (posedge clk or negedge clrn)
	begin
		if (~clrn)
		begin
			mem0 <= 0;
			mem1 <= 0;
			mem2 <= 0;
			mem3 <= 0;
			mem4 <= 0;
			mem5 <= 0;
			mem6 <= 0;
			mem7 <= 0;
			mem8 <= 0;
			mem9 <= 0;
			mem10 <= 0;
			mem11 <= 0;
			mem12 <= 0;
			mem13 <= 0;
			mem14 <= 0;
			mem15 <= 0;
			mem16 <= 0;
			mem17 <= 0;
			mem18 <= 0;
			mem19 <= 0;
			mem20 <= 0;
			mem21 <= 0;
			mem22 <= 0;
			mem23 <= 0;
			mem24 <= 0;
			mem25 <= 0;
			mem26 <= 0;
			mem27 <= 0;
			mem28 <= 0;
			mem29 <= 0;
			mem30 <= 0;
			mem31 <= 0;
		end
		else if (enable)
		begin
			mem31 <= mem30 ^ scale31;
			mem30 <= mem29 ^ scale30;
			mem29 <= mem28 ^ scale29;
			mem28 <= mem27 ^ scale28;
			mem27 <= mem26 ^ scale27;
			mem26 <= mem25 ^ scale26;
			mem25 <= mem24 ^ scale25;
			mem24 <= mem23 ^ scale24;
			mem23 <= mem22 ^ scale23;
			mem22 <= mem21 ^ scale22;
			mem21 <= mem20 ^ scale21;
			mem20 <= mem19 ^ scale20;
			mem19 <= mem18 ^ scale19;
			mem18 <= mem17 ^ scale18;
			mem17 <= mem16 ^ scale17;
			mem16 <= mem15 ^ scale16;
			mem15 <= mem14 ^ scale15;
			mem14 <= mem13 ^ scale14;
			mem13 <= mem12 ^ scale13;
			mem12 <= mem11 ^ scale12;
			mem11 <= mem10 ^ scale11;
			mem10 <= mem9 ^ scale10;
			mem9 <= mem8 ^ scale9;
			mem8 <= mem7 ^ scale8;
			mem7 <= mem6 ^ scale7;
			mem6 <= mem5 ^ scale6;
			mem5 <= mem4 ^ scale5;
			mem4 <= mem3 ^ scale4;
			mem3 <= mem2 ^ scale3;
			mem2 <= mem1 ^ scale2;
			mem1 <= mem0 ^ scale1;
			mem0 <= scale0;
		end
	end

	always @ (data or x or mem31)
		if (data) feedback = x ^ mem31;
		else feedback = 0;

	always @ (data or x or mem31)
		if (data) y = x;
		else y = mem31;

endmodule
