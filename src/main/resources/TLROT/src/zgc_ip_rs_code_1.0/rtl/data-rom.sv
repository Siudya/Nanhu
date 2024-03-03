// -------------------------------------------------------------------------
// a sample ROM, feeding data to encoder
//Copyright (C) Tue Apr  2 14:40:09 2002
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

module sigin(data, address);
	input [7:0] address;
	output [7:0] data;
	reg [7:0] data;
	
	// an arbitrary data source here
	always @ (address) data = address + 1;
endmodule

