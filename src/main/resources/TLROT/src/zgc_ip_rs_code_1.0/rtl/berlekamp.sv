// -------------------------------------------------------------------------
//Berlekamp circuit for Reed-Solomon decoder
//Copyright (C) Tue Apr  2 17:07:10 2002
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

module rsdec_berl (lambda_out, omega_out, syndrome0, syndrome1, syndrome2, syndrome3, syndrome4, syndrome5, syndrome6, syndrome7, syndrome8, syndrome9, syndrome10, syndrome11, syndrome12, syndrome13, syndrome14, syndrome15, syndrome16, syndrome17, syndrome18, syndrome19, syndrome20, syndrome21, syndrome22, syndrome23, syndrome24, syndrome25, syndrome26, syndrome27, syndrome28, syndrome29, syndrome30, syndrome31, 
		D, DI, count, phase0, phase32, enable, clk, clrn);
	input clk, clrn, enable, phase0, phase32;
	input [7:0] syndrome0;
	input [7:0] syndrome1;
	input [7:0] syndrome2;
	input [7:0] syndrome3;
	input [7:0] syndrome4;
	input [7:0] syndrome5;
	input [7:0] syndrome6;
	input [7:0] syndrome7;
	input [7:0] syndrome8;
	input [7:0] syndrome9;
	input [7:0] syndrome10;
	input [7:0] syndrome11;
	input [7:0] syndrome12;
	input [7:0] syndrome13;
	input [7:0] syndrome14;
	input [7:0] syndrome15;
	input [7:0] syndrome16;
	input [7:0] syndrome17;
	input [7:0] syndrome18;
	input [7:0] syndrome19;
	input [7:0] syndrome20;
	input [7:0] syndrome21;
	input [7:0] syndrome22;
	input [7:0] syndrome23;
	input [7:0] syndrome24;
	input [7:0] syndrome25;
	input [7:0] syndrome26;
	input [7:0] syndrome27;
	input [7:0] syndrome28;
	input [7:0] syndrome29;
	input [7:0] syndrome30;
	input [7:0] syndrome31;
	input [7:0] DI;
	input [5:0] count;
	output [7:0] lambda_out;
	output [7:0] omega_out;
	reg [7:0] lambda_out;
	reg [7:0] omega_out;
	output [7:0] D;
	reg [7:0] D;

	integer j;
	reg init, delta;
	reg [4:0] L;
	reg [7:0] lambda[31:0];
	reg [7:0] omega[31:0];
	reg [7:0] A[30:0];
	reg [7:0] B[30:0];
	wire [7:0] tmp0;
	wire [7:0] tmp1;
	wire [7:0] tmp2;
	wire [7:0] tmp3;
	wire [7:0] tmp4;
	wire [7:0] tmp5;
	wire [7:0] tmp6;
	wire [7:0] tmp7;
	wire [7:0] tmp8;
	wire [7:0] tmp9;
	wire [7:0] tmp10;
	wire [7:0] tmp11;
	wire [7:0] tmp12;
	wire [7:0] tmp13;
	wire [7:0] tmp14;
	wire [7:0] tmp15;
	wire [7:0] tmp16;
	wire [7:0] tmp17;
	wire [7:0] tmp18;
	wire [7:0] tmp19;
	wire [7:0] tmp20;
	wire [7:0] tmp21;
	wire [7:0] tmp22;
	wire [7:0] tmp23;
	wire [7:0] tmp24;
	wire [7:0] tmp25;
	wire [7:0] tmp26;
	wire [7:0] tmp27;
	wire [7:0] tmp28;
	wire [7:0] tmp29;
	wire [7:0] tmp30;
	wire [7:0] tmp31;

	always @ (tmp1) lambda_out = tmp1;
	always @ (tmp3) omega_out = tmp3;

	always @ (L or D or count)
		// delta = (D != 0 && 2*L <= i);
		if (D != 0 && count >= {L, 1'b0}) delta = 1;
		else delta = 0;

	rsdec_berl_multiply x0 (tmp0, B[30], D, lambda[0], syndrome0, phase0);
	rsdec_berl_multiply x1 (tmp1, lambda[31], DI, lambda[1], syndrome1, phase0);
	rsdec_berl_multiply x2 (tmp2, A[30], D, lambda[2], syndrome2, phase0);
	rsdec_berl_multiply x3 (tmp3, omega[31], DI, lambda[3], syndrome3, phase0);
	multiply x4 (tmp4, lambda[4], syndrome4);
	multiply x5 (tmp5, lambda[5], syndrome5);
	multiply x6 (tmp6, lambda[6], syndrome6);
	multiply x7 (tmp7, lambda[7], syndrome7);
	multiply x8 (tmp8, lambda[8], syndrome8);
	multiply x9 (tmp9, lambda[9], syndrome9);
	multiply x10 (tmp10, lambda[10], syndrome10);
	multiply x11 (tmp11, lambda[11], syndrome11);
	multiply x12 (tmp12, lambda[12], syndrome12);
	multiply x13 (tmp13, lambda[13], syndrome13);
	multiply x14 (tmp14, lambda[14], syndrome14);
	multiply x15 (tmp15, lambda[15], syndrome15);
	multiply x16 (tmp16, lambda[16], syndrome16);
	multiply x17 (tmp17, lambda[17], syndrome17);
	multiply x18 (tmp18, lambda[18], syndrome18);
	multiply x19 (tmp19, lambda[19], syndrome19);
	multiply x20 (tmp20, lambda[20], syndrome20);
	multiply x21 (tmp21, lambda[21], syndrome21);
	multiply x22 (tmp22, lambda[22], syndrome22);
	multiply x23 (tmp23, lambda[23], syndrome23);
	multiply x24 (tmp24, lambda[24], syndrome24);
	multiply x25 (tmp25, lambda[25], syndrome25);
	multiply x26 (tmp26, lambda[26], syndrome26);
	multiply x27 (tmp27, lambda[27], syndrome27);
	multiply x28 (tmp28, lambda[28], syndrome28);
	multiply x29 (tmp29, lambda[29], syndrome29);
	multiply x30 (tmp30, lambda[30], syndrome30);
	multiply x31 (tmp31, lambda[31], syndrome31);

	always @ (posedge clk or negedge clrn)
	begin
		// for (j = t-1; j >=0; j--)
		//	if (j != 0) lambda[j] += D * B[j-1];
		if (~clrn)
		begin
			for (j = 0; j < 32; j = j + 1) lambda[j] <= 0;
			for (j = 0; j < 31; j = j + 1) B[j] <= 0;
			for (j = 0; j < 32; j = j + 1) omega[j] <= 0;
			for (j = 0; j < 31; j = j + 1) A[j] <= 0;
			L = 0;
			D = 0;
		end
		else if (~enable)
		begin
			lambda[0] <= 1;
			for (j = 1; j < 32; j = j +1) lambda[j] <= 0;
			B[0] <= 1;
			for (j = 1; j < 31; j = j +1) B[j] <= 0;
			omega[0] <= 1;
			for (j = 1; j < 32; j = j +1) omega[j] <= 0;
			for (j = 0; j < 31; j = j + 1) A[j] <= 0;
			L = 0;
			D = 0;
		end
		else
		begin
			if (~phase0)
			begin
				if (~phase32) lambda[0] <= lambda[31] ^ tmp0;
				else lambda[0] <= lambda[31];
				for (j = 1; j < 32; j = j + 1)
					lambda[j] <= lambda[j-1];
			end

		// for (j = t-1; j >=0; j--)
		//	if (delta) B[j] = lambda[j] *DI;
		//	else if (j != 0) B[j] = B[j-1];
		//	else B[j] = 0;
			if (~phase0)
			begin
				if (delta)	B[0] <= tmp1;
				else if (~phase32) B[0] <= B[30];
				else B[0] <= 0;
				for (j = 1; j < 31; j = j + 1)
					B[j] <= B[j-1];
			end

		// for (j = t-1; j >=0; j--)
		//	if (j != 0) omega[j] += D * A[j-1];
			if (~phase0)
			begin
				if (~phase32) omega[0] <= omega[31] ^ tmp2;
				else omega[0] <= omega[31];
				for (j = 1; j < 32; j = j + 1)
					omega[j] <= omega[j-1];
			end

		// for (j = t-1; j >=0; j--)
		//	if (delta) A[j] = omega[j] *DI;
		//	else if (j != 0) A[j] = A[j-1];
		//	else A[j] = 0;
			if (~phase0)
			begin
				if (delta)	A[0] <= tmp3;
				else if (~phase32) A[0] <= A[30];
				else A[0] <= 0;
				for (j = 1; j < 31; j = j + 1)
					A[j] <= A[j-1];
			end

		// if (delta) L = i - L + 1;
			if ((phase0 & delta) && (count != -1)) L = count - L + 1;

		//for (D = j = 0; j < t; j = j + 1)
		//	D += lambda[j] * syndrome[t-j-1];
			if (phase0)
				D = tmp0 ^ tmp1 ^ tmp2 ^ tmp3 ^ tmp4 ^ tmp5 ^ tmp6 ^ tmp7 ^ tmp8 ^ tmp9 ^ tmp10 ^ tmp11 ^ tmp12 ^ tmp13 ^ tmp14 ^ tmp15 ^ tmp16 ^ tmp17 ^ tmp18 ^ tmp19 ^ tmp20 ^ tmp21 ^ tmp22 ^ tmp23 ^ tmp24 ^ tmp25 ^ tmp26 ^ tmp27 ^ tmp28 ^ tmp29 ^ tmp30 ^ tmp31;

		end
	end

endmodule


module rsdec_berl_multiply (y, a, b, c, d, e);
	input [7:0] a, b, c, d;
	input e;
	output [7:0] y;
	wire [7:0] y;
	reg [7:0] p, q;

	always @ (a or c or e)
		if (e) p = c;
		else p = a;
	always @ (b or d or e)
		if (e) q = d;
		else q = b;

	multiply x0 (y, p, q);

endmodule

module multiply (y, a, b);
	input [7:0] a, b;
	output [7:0] y;
	reg [7:0] y;
	always @ (a or b)
	begin
		y[0] = (a[0] & b[0]) ^ (a[1] & b[7]) ^ (a[2] & b[6]) ^ (a[2] & b[7]) ^ (a[3] & b[5]) ^ (a[3] & b[6]) ^ (a[3] & b[7]) ^ (a[4] & b[4]) ^ (a[4] & b[5]) ^ (a[4] & b[6]) ^ (a[4] & b[7]) ^ (a[5] & b[3]) ^ (a[5] & b[4]) ^ (a[5] & b[5]) ^ (a[5] & b[6]) ^ (a[5] & b[7]) ^ (a[6] & b[2]) ^ (a[6] & b[3]) ^ (a[6] & b[4]) ^ (a[6] & b[5]) ^ (a[6] & b[6]) ^ (a[6] & b[7]) ^ (a[7] & b[1]) ^ (a[7] & b[2]) ^ (a[7] & b[3]) ^ (a[7] & b[4]) ^ (a[7] & b[5]) ^ (a[7] & b[6]);
		y[1] = (a[0] & b[1]) ^ (a[1] & b[0]) ^ (a[1] & b[7]) ^ (a[2] & b[6]) ^ (a[3] & b[5]) ^ (a[4] & b[4]) ^ (a[5] & b[3]) ^ (a[6] & b[2]) ^ (a[7] & b[1]) ^ (a[7] & b[7]);
		y[2] = (a[0] & b[2]) ^ (a[1] & b[1]) ^ (a[1] & b[7]) ^ (a[2] & b[0]) ^ (a[2] & b[6]) ^ (a[3] & b[5]) ^ (a[3] & b[7]) ^ (a[4] & b[4]) ^ (a[4] & b[6]) ^ (a[4] & b[7]) ^ (a[5] & b[3]) ^ (a[5] & b[5]) ^ (a[5] & b[6]) ^ (a[5] & b[7]) ^ (a[6] & b[2]) ^ (a[6] & b[4]) ^ (a[6] & b[5]) ^ (a[6] & b[6]) ^ (a[6] & b[7]) ^ (a[7] & b[1]) ^ (a[7] & b[3]) ^ (a[7] & b[4]) ^ (a[7] & b[5]) ^ (a[7] & b[6]);
		y[3] = (a[0] & b[3]) ^ (a[1] & b[2]) ^ (a[2] & b[1]) ^ (a[2] & b[7]) ^ (a[3] & b[0]) ^ (a[3] & b[6]) ^ (a[4] & b[5]) ^ (a[4] & b[7]) ^ (a[5] & b[4]) ^ (a[5] & b[6]) ^ (a[5] & b[7]) ^ (a[6] & b[3]) ^ (a[6] & b[5]) ^ (a[6] & b[6]) ^ (a[6] & b[7]) ^ (a[7] & b[2]) ^ (a[7] & b[4]) ^ (a[7] & b[5]) ^ (a[7] & b[6]) ^ (a[7] & b[7]);
		y[4] = (a[0] & b[4]) ^ (a[1] & b[3]) ^ (a[2] & b[2]) ^ (a[3] & b[1]) ^ (a[3] & b[7]) ^ (a[4] & b[0]) ^ (a[4] & b[6]) ^ (a[5] & b[5]) ^ (a[5] & b[7]) ^ (a[6] & b[4]) ^ (a[6] & b[6]) ^ (a[6] & b[7]) ^ (a[7] & b[3]) ^ (a[7] & b[5]) ^ (a[7] & b[6]) ^ (a[7] & b[7]);
		y[5] = (a[0] & b[5]) ^ (a[1] & b[4]) ^ (a[2] & b[3]) ^ (a[3] & b[2]) ^ (a[4] & b[1]) ^ (a[4] & b[7]) ^ (a[5] & b[0]) ^ (a[5] & b[6]) ^ (a[6] & b[5]) ^ (a[6] & b[7]) ^ (a[7] & b[4]) ^ (a[7] & b[6]) ^ (a[7] & b[7]);
		y[6] = (a[0] & b[6]) ^ (a[1] & b[5]) ^ (a[2] & b[4]) ^ (a[3] & b[3]) ^ (a[4] & b[2]) ^ (a[5] & b[1]) ^ (a[5] & b[7]) ^ (a[6] & b[0]) ^ (a[6] & b[6]) ^ (a[7] & b[5]) ^ (a[7] & b[7]);
		y[7] = (a[0] & b[7]) ^ (a[1] & b[6]) ^ (a[1] & b[7]) ^ (a[2] & b[5]) ^ (a[2] & b[6]) ^ (a[2] & b[7]) ^ (a[3] & b[4]) ^ (a[3] & b[5]) ^ (a[3] & b[6]) ^ (a[3] & b[7]) ^ (a[4] & b[3]) ^ (a[4] & b[4]) ^ (a[4] & b[5]) ^ (a[4] & b[6]) ^ (a[4] & b[7]) ^ (a[5] & b[2]) ^ (a[5] & b[3]) ^ (a[5] & b[4]) ^ (a[5] & b[5]) ^ (a[5] & b[6]) ^ (a[5] & b[7]) ^ (a[6] & b[1]) ^ (a[6] & b[2]) ^ (a[6] & b[3]) ^ (a[6] & b[4]) ^ (a[6] & b[5]) ^ (a[6] & b[6]) ^ (a[7] & b[0]) ^ (a[7] & b[1]) ^ (a[7] & b[2]) ^ (a[7] & b[3]) ^ (a[7] & b[4]) ^ (a[7] & b[5]);
	end
endmodule

