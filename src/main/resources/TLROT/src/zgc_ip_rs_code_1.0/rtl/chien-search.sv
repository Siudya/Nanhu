// -------------------------------------------------------------------------
//Chien-Forney search circuit for Reed-Solomon decoder
//Copyright (C) Tue Apr  2 17:07:16 2002
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

module rsdec_chien_scale0 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0];
		y[1] = x[1];
		y[2] = x[2];
		y[3] = x[3];
		y[4] = x[4];
		y[5] = x[5];
		y[6] = x[6];
		y[7] = x[7];
	end
endmodule

module rsdec_chien_scale1 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[7];
		y[1] = x[0] ^ x[7];
		y[2] = x[1] ^ x[7];
		y[3] = x[2];
		y[4] = x[3];
		y[5] = x[4];
		y[6] = x[5];
		y[7] = x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale2 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[6] ^ x[7];
		y[1] = x[6];
		y[2] = x[0] ^ x[6];
		y[3] = x[1] ^ x[7];
		y[4] = x[2];
		y[5] = x[3];
		y[6] = x[4];
		y[7] = x[5] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale3 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[5] ^ x[6] ^ x[7];
		y[1] = x[5];
		y[2] = x[5] ^ x[7];
		y[3] = x[0] ^ x[6];
		y[4] = x[1] ^ x[7];
		y[5] = x[2];
		y[6] = x[3];
		y[7] = x[4] ^ x[5] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale4 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[4] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[4];
		y[2] = x[4] ^ x[6] ^ x[7];
		y[3] = x[5] ^ x[7];
		y[4] = x[0] ^ x[6];
		y[5] = x[1] ^ x[7];
		y[6] = x[2];
		y[7] = x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale5 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[3];
		y[2] = x[3] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[4] ^ x[6] ^ x[7];
		y[4] = x[5] ^ x[7];
		y[5] = x[0] ^ x[6];
		y[6] = x[1] ^ x[7];
		y[7] = x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale6 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[2];
		y[2] = x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[3] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[4] ^ x[6] ^ x[7];
		y[5] = x[5] ^ x[7];
		y[6] = x[0] ^ x[6];
		y[7] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
	end
endmodule

module rsdec_chien_scale7 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[1] = x[1] ^ x[7];
		y[2] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[3] = x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[3] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[4] ^ x[6] ^ x[7];
		y[6] = x[5] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
	end
endmodule

module rsdec_chien_scale8 (y, x);
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

module rsdec_chien_scale9 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[7];
		y[1] = x[5] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[4] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[5] = x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[3] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[6];
	end
endmodule

module rsdec_chien_scale10 (y, x);
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

module rsdec_chien_scale11 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[5] ^ x[7];
		y[1] = x[3] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[6] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[1] ^ x[4] ^ x[6];
	end
endmodule

module rsdec_chien_scale12 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[4] ^ x[6];
		y[1] = x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[6] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[7] = x[0] ^ x[3] ^ x[5];
	end
endmodule

module rsdec_chien_scale13 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[3] ^ x[5];
		y[1] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[7] = x[2] ^ x[4] ^ x[7];
	end
endmodule

module rsdec_chien_scale14 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[2] ^ x[4] ^ x[7];
		y[1] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[1] ^ x[3] ^ x[6];
	end
endmodule

module rsdec_chien_scale15 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[1] ^ x[3] ^ x[6];
		y[1] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[2] ^ x[5] ^ x[7];
	end
endmodule

module rsdec_chien_scale16 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[2] ^ x[5] ^ x[7];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[6] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[7] = x[1] ^ x[4] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale17 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[1] ^ x[4] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[3] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[3] ^ x[5] ^ x[6];
	end
endmodule

module rsdec_chien_scale18 (y, x);
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

module rsdec_chien_scale19 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[2] ^ x[4] ^ x[5] ^ x[7];
		y[1] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[3];
		y[3] = x[1] ^ x[2] ^ x[3] ^ x[4];
		y[4] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[5] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[1] ^ x[3] ^ x[4] ^ x[6];
	end
endmodule

module rsdec_chien_scale20 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[1] ^ x[3] ^ x[4] ^ x[6];
		y[1] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[7];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[3];
		y[4] = x[1] ^ x[2] ^ x[3] ^ x[4];
		y[5] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[6] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[2] ^ x[3] ^ x[5] ^ x[7];
	end
endmodule

module rsdec_chien_scale21 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[2] ^ x[3] ^ x[5] ^ x[7];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[6];
		y[3] = x[0] ^ x[1] ^ x[2] ^ x[7];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[3];
		y[5] = x[1] ^ x[2] ^ x[3] ^ x[4];
		y[6] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[7] = x[1] ^ x[2] ^ x[4] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale22 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[1] ^ x[2] ^ x[4] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[2] = x[0] ^ x[5];
		y[3] = x[0] ^ x[1] ^ x[6];
		y[4] = x[0] ^ x[1] ^ x[2] ^ x[7];
		y[5] = x[0] ^ x[1] ^ x[2] ^ x[3];
		y[6] = x[1] ^ x[2] ^ x[3] ^ x[4];
		y[7] = x[0] ^ x[1] ^ x[3] ^ x[5] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale23 (y, x);
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

module rsdec_chien_scale24 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[2] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[1] ^ x[2] ^ x[3] ^ x[4];
		y[2] = x[3] ^ x[6] ^ x[7];
		y[3] = x[4] ^ x[7];
		y[4] = x[0] ^ x[5];
		y[5] = x[0] ^ x[1] ^ x[6];
		y[6] = x[0] ^ x[1] ^ x[2] ^ x[7];
		y[7] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
	end
endmodule

module rsdec_chien_scale25 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[1] ^ x[3] ^ x[4] ^ x[5] ^ x[6] ^ x[7];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[3];
		y[2] = x[2] ^ x[5] ^ x[6] ^ x[7];
		y[3] = x[3] ^ x[6] ^ x[7];
		y[4] = x[4] ^ x[7];
		y[5] = x[0] ^ x[5];
		y[6] = x[0] ^ x[1] ^ x[6];
		y[7] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
	end
endmodule

module rsdec_chien_scale26 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[2] ^ x[3] ^ x[4] ^ x[5] ^ x[6];
		y[1] = x[0] ^ x[1] ^ x[2] ^ x[7];
		y[2] = x[1] ^ x[4] ^ x[5] ^ x[6];
		y[3] = x[2] ^ x[5] ^ x[6] ^ x[7];
		y[4] = x[3] ^ x[6] ^ x[7];
		y[5] = x[4] ^ x[7];
		y[6] = x[0] ^ x[5];
		y[7] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
	end
endmodule

module rsdec_chien_scale27 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[1] ^ x[2] ^ x[3] ^ x[4] ^ x[5];
		y[1] = x[0] ^ x[1] ^ x[6];
		y[2] = x[0] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[3] = x[1] ^ x[4] ^ x[5] ^ x[6];
		y[4] = x[2] ^ x[5] ^ x[6] ^ x[7];
		y[5] = x[3] ^ x[6] ^ x[7];
		y[6] = x[4] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4];
	end
endmodule

module rsdec_chien_scale28 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[4];
		y[1] = x[0] ^ x[5];
		y[2] = x[2] ^ x[3] ^ x[4] ^ x[6];
		y[3] = x[0] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[4] = x[1] ^ x[4] ^ x[5] ^ x[6];
		y[5] = x[2] ^ x[5] ^ x[6] ^ x[7];
		y[6] = x[3] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[7];
	end
endmodule

module rsdec_chien_scale29 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[3] ^ x[7];
		y[1] = x[4] ^ x[7];
		y[2] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[7];
		y[3] = x[2] ^ x[3] ^ x[4] ^ x[6];
		y[4] = x[0] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[5] = x[1] ^ x[4] ^ x[5] ^ x[6];
		y[6] = x[2] ^ x[5] ^ x[6] ^ x[7];
		y[7] = x[0] ^ x[1] ^ x[2] ^ x[6];
	end
endmodule

module rsdec_chien_scale30 (y, x);
	input [7:0] x;
	output [7:0] y;
	reg [7:0] y;

	always @ (x)
	begin
		y[0] = x[0] ^ x[1] ^ x[2] ^ x[6];
		y[1] = x[3] ^ x[6] ^ x[7];
		y[2] = x[0] ^ x[1] ^ x[2] ^ x[4] ^ x[6] ^ x[7];
		y[3] = x[1] ^ x[2] ^ x[3] ^ x[5] ^ x[7];
		y[4] = x[2] ^ x[3] ^ x[4] ^ x[6];
		y[5] = x[0] ^ x[3] ^ x[4] ^ x[5] ^ x[7];
		y[6] = x[1] ^ x[4] ^ x[5] ^ x[6];
		y[7] = x[0] ^ x[1] ^ x[5] ^ x[7];
	end
endmodule

module rsdec_chien_scale31 (y, x);
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

module rsdec_chien (error, alpha, lambda, omega, even, D, search, load, shorten, clk, clrn);
	input clk, clrn, load, search, shorten;
	input [7:0] D;
	input [7:0] lambda;
	input [7:0] omega;
	output [7:0] even, error;
	output [7:0] alpha;
	reg [7:0] even, error;
	reg [7:0] alpha;

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
	wire [7:0] scale32;
	wire [7:0] scale33;
	wire [7:0] scale34;
	wire [7:0] scale35;
	wire [7:0] scale36;
	wire [7:0] scale37;
	wire [7:0] scale38;
	wire [7:0] scale39;
	wire [7:0] scale40;
	wire [7:0] scale41;
	wire [7:0] scale42;
	wire [7:0] scale43;
	wire [7:0] scale44;
	wire [7:0] scale45;
	wire [7:0] scale46;
	wire [7:0] scale47;
	wire [7:0] scale48;
	wire [7:0] scale49;
	wire [7:0] scale50;
	wire [7:0] scale51;
	wire [7:0] scale52;
	wire [7:0] scale53;
	wire [7:0] scale54;
	wire [7:0] scale55;
	wire [7:0] scale56;
	wire [7:0] scale57;
	wire [7:0] scale58;
	wire [7:0] scale59;
	wire [7:0] scale60;
	wire [7:0] scale61;
	wire [7:0] scale62;
	wire [7:0] scale63;
	reg [7:0] data0;
	reg [7:0] data1;
	reg [7:0] data2;
	reg [7:0] data3;
	reg [7:0] data4;
	reg [7:0] data5;
	reg [7:0] data6;
	reg [7:0] data7;
	reg [7:0] data8;
	reg [7:0] data9;
	reg [7:0] data10;
	reg [7:0] data11;
	reg [7:0] data12;
	reg [7:0] data13;
	reg [7:0] data14;
	reg [7:0] data15;
	reg [7:0] data16;
	reg [7:0] data17;
	reg [7:0] data18;
	reg [7:0] data19;
	reg [7:0] data20;
	reg [7:0] data21;
	reg [7:0] data22;
	reg [7:0] data23;
	reg [7:0] data24;
	reg [7:0] data25;
	reg [7:0] data26;
	reg [7:0] data27;
	reg [7:0] data28;
	reg [7:0] data29;
	reg [7:0] data30;
	reg [7:0] data31;
	reg [7:0] a0;
	reg [7:0] a1;
	reg [7:0] a2;
	reg [7:0] a3;
	reg [7:0] a4;
	reg [7:0] a5;
	reg [7:0] a6;
	reg [7:0] a7;
	reg [7:0] a8;
	reg [7:0] a9;
	reg [7:0] a10;
	reg [7:0] a11;
	reg [7:0] a12;
	reg [7:0] a13;
	reg [7:0] a14;
	reg [7:0] a15;
	reg [7:0] a16;
	reg [7:0] a17;
	reg [7:0] a18;
	reg [7:0] a19;
	reg [7:0] a20;
	reg [7:0] a21;
	reg [7:0] a22;
	reg [7:0] a23;
	reg [7:0] a24;
	reg [7:0] a25;
	reg [7:0] a26;
	reg [7:0] a27;
	reg [7:0] a28;
	reg [7:0] a29;
	reg [7:0] a30;
	reg [7:0] a31;
	reg [7:0] l0;
	reg [7:0] l1;
	reg [7:0] l2;
	reg [7:0] l3;
	reg [7:0] l4;
	reg [7:0] l5;
	reg [7:0] l6;
	reg [7:0] l7;
	reg [7:0] l8;
	reg [7:0] l9;
	reg [7:0] l10;
	reg [7:0] l11;
	reg [7:0] l12;
	reg [7:0] l13;
	reg [7:0] l14;
	reg [7:0] l15;
	reg [7:0] l16;
	reg [7:0] l17;
	reg [7:0] l18;
	reg [7:0] l19;
	reg [7:0] l20;
	reg [7:0] l21;
	reg [7:0] l22;
	reg [7:0] l23;
	reg [7:0] l24;
	reg [7:0] l25;
	reg [7:0] l26;
	reg [7:0] l27;
	reg [7:0] l28;
	reg [7:0] l29;
	reg [7:0] l30;
	reg [7:0] l31;
	reg [7:0] o0;
	reg [7:0] o1;
	reg [7:0] o2;
	reg [7:0] o3;
	reg [7:0] o4;
	reg [7:0] o5;
	reg [7:0] o6;
	reg [7:0] o7;
	reg [7:0] o8;
	reg [7:0] o9;
	reg [7:0] o10;
	reg [7:0] o11;
	reg [7:0] o12;
	reg [7:0] o13;
	reg [7:0] o14;
	reg [7:0] o15;
	reg [7:0] o16;
	reg [7:0] o17;
	reg [7:0] o18;
	reg [7:0] o19;
	reg [7:0] o20;
	reg [7:0] o21;
	reg [7:0] o22;
	reg [7:0] o23;
	reg [7:0] o24;
	reg [7:0] o25;
	reg [7:0] o26;
	reg [7:0] o27;
	reg [7:0] o28;
	reg [7:0] o29;
	reg [7:0] o30;
	reg [7:0] o31;
	reg [7:0] odd, numerator;
	wire [7:0] tmp;
	integer j;

	rsdec_chien_scale0 x0 (scale0, data0);
	rsdec_chien_scale1 x1 (scale1, data1);
	rsdec_chien_scale2 x2 (scale2, data2);
	rsdec_chien_scale3 x3 (scale3, data3);
	rsdec_chien_scale4 x4 (scale4, data4);
	rsdec_chien_scale5 x5 (scale5, data5);
	rsdec_chien_scale6 x6 (scale6, data6);
	rsdec_chien_scale7 x7 (scale7, data7);
	rsdec_chien_scale8 x8 (scale8, data8);
	rsdec_chien_scale9 x9 (scale9, data9);
	rsdec_chien_scale10 x10 (scale10, data10);
	rsdec_chien_scale11 x11 (scale11, data11);
	rsdec_chien_scale12 x12 (scale12, data12);
	rsdec_chien_scale13 x13 (scale13, data13);
	rsdec_chien_scale14 x14 (scale14, data14);
	rsdec_chien_scale15 x15 (scale15, data15);
	rsdec_chien_scale16 x16 (scale16, data16);
	rsdec_chien_scale17 x17 (scale17, data17);
	rsdec_chien_scale18 x18 (scale18, data18);
	rsdec_chien_scale19 x19 (scale19, data19);
	rsdec_chien_scale20 x20 (scale20, data20);
	rsdec_chien_scale21 x21 (scale21, data21);
	rsdec_chien_scale22 x22 (scale22, data22);
	rsdec_chien_scale23 x23 (scale23, data23);
	rsdec_chien_scale24 x24 (scale24, data24);
	rsdec_chien_scale25 x25 (scale25, data25);
	rsdec_chien_scale26 x26 (scale26, data26);
	rsdec_chien_scale27 x27 (scale27, data27);
	rsdec_chien_scale28 x28 (scale28, data28);
	rsdec_chien_scale29 x29 (scale29, data29);
	rsdec_chien_scale30 x30 (scale30, data30);
	rsdec_chien_scale31 x31 (scale31, data31);
	rsdec_chien_scale0 x32 (scale32, o0);
	rsdec_chien_scale1 x33 (scale33, o1);
	rsdec_chien_scale2 x34 (scale34, o2);
	rsdec_chien_scale3 x35 (scale35, o3);
	rsdec_chien_scale4 x36 (scale36, o4);
	rsdec_chien_scale5 x37 (scale37, o5);
	rsdec_chien_scale6 x38 (scale38, o6);
	rsdec_chien_scale7 x39 (scale39, o7);
	rsdec_chien_scale8 x40 (scale40, o8);
	rsdec_chien_scale9 x41 (scale41, o9);
	rsdec_chien_scale10 x42 (scale42, o10);
	rsdec_chien_scale11 x43 (scale43, o11);
	rsdec_chien_scale12 x44 (scale44, o12);
	rsdec_chien_scale13 x45 (scale45, o13);
	rsdec_chien_scale14 x46 (scale46, o14);
	rsdec_chien_scale15 x47 (scale47, o15);
	rsdec_chien_scale16 x48 (scale48, o16);
	rsdec_chien_scale17 x49 (scale49, o17);
	rsdec_chien_scale18 x50 (scale50, o18);
	rsdec_chien_scale19 x51 (scale51, o19);
	rsdec_chien_scale20 x52 (scale52, o20);
	rsdec_chien_scale21 x53 (scale53, o21);
	rsdec_chien_scale22 x54 (scale54, o22);
	rsdec_chien_scale23 x55 (scale55, o23);
	rsdec_chien_scale24 x56 (scale56, o24);
	rsdec_chien_scale25 x57 (scale57, o25);
	rsdec_chien_scale26 x58 (scale58, o26);
	rsdec_chien_scale27 x59 (scale59, o27);
	rsdec_chien_scale28 x60 (scale60, o28);
	rsdec_chien_scale29 x61 (scale61, o29);
	rsdec_chien_scale30 x62 (scale62, o30);
	rsdec_chien_scale31 x63 (scale63, o31);

	always @ (shorten or a0 or l0)
		if (shorten) data0 = a0;
		else data0 = l0;

	always @ (shorten or a1 or l1)
		if (shorten) data1 = a1;
		else data1 = l1;

	always @ (shorten or a2 or l2)
		if (shorten) data2 = a2;
		else data2 = l2;

	always @ (shorten or a3 or l3)
		if (shorten) data3 = a3;
		else data3 = l3;

	always @ (shorten or a4 or l4)
		if (shorten) data4 = a4;
		else data4 = l4;

	always @ (shorten or a5 or l5)
		if (shorten) data5 = a5;
		else data5 = l5;

	always @ (shorten or a6 or l6)
		if (shorten) data6 = a6;
		else data6 = l6;

	always @ (shorten or a7 or l7)
		if (shorten) data7 = a7;
		else data7 = l7;

	always @ (shorten or a8 or l8)
		if (shorten) data8 = a8;
		else data8 = l8;

	always @ (shorten or a9 or l9)
		if (shorten) data9 = a9;
		else data9 = l9;

	always @ (shorten or a10 or l10)
		if (shorten) data10 = a10;
		else data10 = l10;

	always @ (shorten or a11 or l11)
		if (shorten) data11 = a11;
		else data11 = l11;

	always @ (shorten or a12 or l12)
		if (shorten) data12 = a12;
		else data12 = l12;

	always @ (shorten or a13 or l13)
		if (shorten) data13 = a13;
		else data13 = l13;

	always @ (shorten or a14 or l14)
		if (shorten) data14 = a14;
		else data14 = l14;

	always @ (shorten or a15 or l15)
		if (shorten) data15 = a15;
		else data15 = l15;

	always @ (shorten or a16 or l16)
		if (shorten) data16 = a16;
		else data16 = l16;

	always @ (shorten or a17 or l17)
		if (shorten) data17 = a17;
		else data17 = l17;

	always @ (shorten or a18 or l18)
		if (shorten) data18 = a18;
		else data18 = l18;

	always @ (shorten or a19 or l19)
		if (shorten) data19 = a19;
		else data19 = l19;

	always @ (shorten or a20 or l20)
		if (shorten) data20 = a20;
		else data20 = l20;

	always @ (shorten or a21 or l21)
		if (shorten) data21 = a21;
		else data21 = l21;

	always @ (shorten or a22 or l22)
		if (shorten) data22 = a22;
		else data22 = l22;

	always @ (shorten or a23 or l23)
		if (shorten) data23 = a23;
		else data23 = l23;

	always @ (shorten or a24 or l24)
		if (shorten) data24 = a24;
		else data24 = l24;

	always @ (shorten or a25 or l25)
		if (shorten) data25 = a25;
		else data25 = l25;

	always @ (shorten or a26 or l26)
		if (shorten) data26 = a26;
		else data26 = l26;

	always @ (shorten or a27 or l27)
		if (shorten) data27 = a27;
		else data27 = l27;

	always @ (shorten or a28 or l28)
		if (shorten) data28 = a28;
		else data28 = l28;

	always @ (shorten or a29 or l29)
		if (shorten) data29 = a29;
		else data29 = l29;

	always @ (shorten or a30 or l30)
		if (shorten) data30 = a30;
		else data30 = l30;

	always @ (shorten or a31 or l31)
		if (shorten) data31 = a31;
		else data31 = l31;

	always @ (posedge clk or negedge clrn)
	begin
		if (~clrn)
		begin
			l0 <= 0;
			l1 <= 0;
			l2 <= 0;
			l3 <= 0;
			l4 <= 0;
			l5 <= 0;
			l6 <= 0;
			l7 <= 0;
			l8 <= 0;
			l9 <= 0;
			l10 <= 0;
			l11 <= 0;
			l12 <= 0;
			l13 <= 0;
			l14 <= 0;
			l15 <= 0;
			l16 <= 0;
			l17 <= 0;
			l18 <= 0;
			l19 <= 0;
			l20 <= 0;
			l21 <= 0;
			l22 <= 0;
			l23 <= 0;
			l24 <= 0;
			l25 <= 0;
			l26 <= 0;
			l27 <= 0;
			l28 <= 0;
			l29 <= 0;
			l30 <= 0;
			l31 <= 0;
			o0 <= 0;
			o1 <= 0;
			o2 <= 0;
			o3 <= 0;
			o4 <= 0;
			o5 <= 0;
			o6 <= 0;
			o7 <= 0;
			o8 <= 0;
			o9 <= 0;
			o10 <= 0;
			o11 <= 0;
			o12 <= 0;
			o13 <= 0;
			o14 <= 0;
			o15 <= 0;
			o16 <= 0;
			o17 <= 0;
			o18 <= 0;
			o19 <= 0;
			o20 <= 0;
			o21 <= 0;
			o22 <= 0;
			o23 <= 0;
			o24 <= 0;
			o25 <= 0;
			o26 <= 0;
			o27 <= 0;
			o28 <= 0;
			o29 <= 0;
			o30 <= 0;
			o31 <= 0;
			a0 <= 1;
			a1 <= 1;
			a2 <= 1;
			a3 <= 1;
			a4 <= 1;
			a5 <= 1;
			a6 <= 1;
			a7 <= 1;
			a8 <= 1;
			a9 <= 1;
			a10 <= 1;
			a11 <= 1;
			a12 <= 1;
			a13 <= 1;
			a14 <= 1;
			a15 <= 1;
			a16 <= 1;
			a17 <= 1;
			a18 <= 1;
			a19 <= 1;
			a20 <= 1;
			a21 <= 1;
			a22 <= 1;
			a23 <= 1;
			a24 <= 1;
			a25 <= 1;
			a26 <= 1;
			a27 <= 1;
			a28 <= 1;
			a29 <= 1;
			a30 <= 1;
			a31 <= 1;
		end
		else if (shorten)
		begin
			a0 <= scale0;
			a1 <= scale1;
			a2 <= scale2;
			a3 <= scale3;
			a4 <= scale4;
			a5 <= scale5;
			a6 <= scale6;
			a7 <= scale7;
			a8 <= scale8;
			a9 <= scale9;
			a10 <= scale10;
			a11 <= scale11;
			a12 <= scale12;
			a13 <= scale13;
			a14 <= scale14;
			a15 <= scale15;
			a16 <= scale16;
			a17 <= scale17;
			a18 <= scale18;
			a19 <= scale19;
			a20 <= scale20;
			a21 <= scale21;
			a22 <= scale22;
			a23 <= scale23;
			a24 <= scale24;
			a25 <= scale25;
			a26 <= scale26;
			a27 <= scale27;
			a28 <= scale28;
			a29 <= scale29;
			a30 <= scale30;
			a31 <= scale31;
		end
		else if (search)
		begin
			l0 <= scale0;
			l1 <= scale1;
			l2 <= scale2;
			l3 <= scale3;
			l4 <= scale4;
			l5 <= scale5;
			l6 <= scale6;
			l7 <= scale7;
			l8 <= scale8;
			l9 <= scale9;
			l10 <= scale10;
			l11 <= scale11;
			l12 <= scale12;
			l13 <= scale13;
			l14 <= scale14;
			l15 <= scale15;
			l16 <= scale16;
			l17 <= scale17;
			l18 <= scale18;
			l19 <= scale19;
			l20 <= scale20;
			l21 <= scale21;
			l22 <= scale22;
			l23 <= scale23;
			l24 <= scale24;
			l25 <= scale25;
			l26 <= scale26;
			l27 <= scale27;
			l28 <= scale28;
			l29 <= scale29;
			l30 <= scale30;
			l31 <= scale31;
			o0 <= scale32;
			o1 <= scale33;
			o2 <= scale34;
			o3 <= scale35;
			o4 <= scale36;
			o5 <= scale37;
			o6 <= scale38;
			o7 <= scale39;
			o8 <= scale40;
			o9 <= scale41;
			o10 <= scale42;
			o11 <= scale43;
			o12 <= scale44;
			o13 <= scale45;
			o14 <= scale46;
			o15 <= scale47;
			o16 <= scale48;
			o17 <= scale49;
			o18 <= scale50;
			o19 <= scale51;
			o20 <= scale52;
			o21 <= scale53;
			o22 <= scale54;
			o23 <= scale55;
			o24 <= scale56;
			o25 <= scale57;
			o26 <= scale58;
			o27 <= scale59;
			o28 <= scale60;
			o29 <= scale61;
			o30 <= scale62;
			o31 <= scale63;
		end
		else if (load)
		begin
			l0 <= lambda;
			l1 <= l0;
			l2 <= l1;
			l3 <= l2;
			l4 <= l3;
			l5 <= l4;
			l6 <= l5;
			l7 <= l6;
			l8 <= l7;
			l9 <= l8;
			l10 <= l9;
			l11 <= l10;
			l12 <= l11;
			l13 <= l12;
			l14 <= l13;
			l15 <= l14;
			l16 <= l15;
			l17 <= l16;
			l18 <= l17;
			l19 <= l18;
			l20 <= l19;
			l21 <= l20;
			l22 <= l21;
			l23 <= l22;
			l24 <= l23;
			l25 <= l24;
			l26 <= l25;
			l27 <= l26;
			l28 <= l27;
			l29 <= l28;
			l30 <= l29;
			l31 <= l30;
			o0 <= omega;
			o1 <= o0;
			o2 <= o1;
			o3 <= o2;
			o4 <= o3;
			o5 <= o4;
			o6 <= o5;
			o7 <= o6;
			o8 <= o7;
			o9 <= o8;
			o10 <= o9;
			o11 <= o10;
			o12 <= o11;
			o13 <= o12;
			o14 <= o13;
			o15 <= o14;
			o16 <= o15;
			o17 <= o16;
			o18 <= o17;
			o19 <= o18;
			o20 <= o19;
			o21 <= o20;
			o22 <= o21;
			o23 <= o22;
			o24 <= o23;
			o25 <= o24;
			o26 <= o25;
			o27 <= o26;
			o28 <= o27;
			o29 <= o28;
			o30 <= o29;
			o31 <= o30;
			a0 <= a31;
			a1 <= a0;
			a2 <= a1;
			a3 <= a2;
			a4 <= a3;
			a5 <= a4;
			a6 <= a5;
			a7 <= a6;
			a8 <= a7;
			a9 <= a8;
			a10 <= a9;
			a11 <= a10;
			a12 <= a11;
			a13 <= a12;
			a14 <= a13;
			a15 <= a14;
			a16 <= a15;
			a17 <= a16;
			a18 <= a17;
			a19 <= a18;
			a20 <= a19;
			a21 <= a20;
			a22 <= a21;
			a23 <= a22;
			a24 <= a23;
			a25 <= a24;
			a26 <= a25;
			a27 <= a26;
			a28 <= a27;
			a29 <= a28;
			a30 <= a29;
			a31 <= a30;
		end
	end

	always @ (l0 or l2 or l4 or l6 or l8 or l10 or l12 or l14 or l16 or l18 or l20 or l22 or l24 or l26 or l28 or l30)
		even = l0 ^ l2 ^ l4 ^ l6 ^ l8 ^ l10 ^ l12 ^ l14 ^ l16 ^ l18 ^ l20 ^ l22 ^ l24 ^ l26 ^ l28 ^ l30;

	always @ (l1 or l3 or l5 or l7 or l9 or l11 or l13 or l15 or l17 or l19 or l21 or l23 or l25 or l27 or l29 or l31)
		odd = l1 ^ l3 ^ l5 ^ l7 ^ l9 ^ l11 ^ l13 ^ l15 ^ l17 ^ l19 ^ l21 ^ l23 ^ l25 ^ l27 ^ l29 ^ l31;

	always @ (o0 or o1 or o2 or o3 or o4 or o5 or o6 or o7 or o8 or o9 or o10 or o11 or o12 or o13 or o14 or o15 or o16 or o17 or o18 or o19 or o20 or o21 or o22 or o23 or o24 or o25 or o26 or o27 or o28 or o29 or o30 or o31)
		numerator = o0 ^ o1 ^ o2 ^ o3 ^ o4 ^ o5 ^ o6 ^ o7 ^ o8 ^ o9 ^ o10 ^ o11 ^ o12 ^ o13 ^ o14 ^ o15 ^ o16 ^ o17 ^ o18 ^ o19 ^ o20 ^ o21 ^ o22 ^ o23 ^ o24 ^ o25 ^ o26 ^ o27 ^ o28 ^ o29 ^ o30 ^ o31;

	multiply m0 (tmp, numerator, D);

	always @ (even or odd or tmp)
		if (even == odd) error = tmp;
		else error = 0;

	always @ (a31) alpha = a31;

endmodule

