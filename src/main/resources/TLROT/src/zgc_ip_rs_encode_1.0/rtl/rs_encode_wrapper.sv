module rs_encode_wrapper(
    input clk,
    input rst_n,
    input clrn,
    input encode_en,               // 开始编码的外部信号
    input [8*168-1:0] datain,
    output reg [8*200-1:0] encoded_data,
    output reg valid,              // 编码完成的信号
    output reg ready,               // 可以进行编码的信号
    
    output     ready_re, 
    output     valid_re,
    output [49:0] encoded_data_re
);

    // Internal signals
    reg [7:0] message;
    reg enc_ena;
    reg data_present;
    wire [7:0] encoded;
    reg [7:0] data_buffer[0:167]; // Buffer to hold input data
    integer i, j;

    assign valid_re = valid;
    assign ready_re = 1'b1;
    assign encoded_data_re = {50{valid}};
     
    // Instantiate the rs_enc module
    rs_enc x1 (
        .y(encoded),
        .x(message),
        .enable(enc_ena),
        .data(data_present),
        .clk(clk),
        .clrn(rst_n & clrn)
    );

    // State machine for control logic
    reg [3:0] state;
    localparam S_IDLE = 0, S_LOAD = 1, S_ENCODE = 2, S_FINISH = 3;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n | !clrn) begin
            // Reset logic
            state <= S_IDLE;
            i <= 0;
            j <= 0;
            enc_ena <= 0;
            data_present <= 0;
            valid <= 0;
            ready <= 1; // Initially ready
            encoded_data <= 0;
            data_buffer[0] <= 0;
            data_buffer[1] <= 0;
            data_buffer[2] <= 0;
            data_buffer[3] <= 0;
            data_buffer[4] <= 0;
            data_buffer[5] <= 0;
            data_buffer[6] <= 0;
            data_buffer[7] <= 0;
            data_buffer[8] <= 0;
            data_buffer[9] <= 0;
            data_buffer[10] <= 0;
            data_buffer[11] <= 0;
            data_buffer[12] <= 0;
            data_buffer[13] <= 0;
            data_buffer[14] <= 0;
            data_buffer[15] <= 0;
            data_buffer[16] <= 0;
            data_buffer[17] <= 0;
            data_buffer[18] <= 0;
            data_buffer[19] <= 0;
            data_buffer[20] <= 0;
            data_buffer[21] <= 0;
            data_buffer[22] <= 0;
            data_buffer[23] <= 0;
            data_buffer[24] <= 0;
            data_buffer[25] <= 0;
            data_buffer[26] <= 0;
            data_buffer[27] <= 0;
            data_buffer[28] <= 0;
            data_buffer[29] <= 0;
            data_buffer[30] <= 0;
            data_buffer[31] <= 0;
            data_buffer[32] <= 0;
            data_buffer[33] <= 0;
            data_buffer[34] <= 0;
            data_buffer[35] <= 0;
            data_buffer[36] <= 0;
            data_buffer[37] <= 0;
            data_buffer[38] <= 0;
            data_buffer[39] <= 0;
            data_buffer[40] <= 0;
            data_buffer[41] <= 0;
            data_buffer[42] <= 0;
            data_buffer[43] <= 0;
            data_buffer[44] <= 0;
            data_buffer[45] <= 0;
            data_buffer[46] <= 0;
            data_buffer[47] <= 0;
            data_buffer[48] <= 0;
            data_buffer[49] <= 0;
            data_buffer[50] <= 0;
            data_buffer[51] <= 0;
            data_buffer[52] <= 0;
            data_buffer[53] <= 0;
            data_buffer[54] <= 0;
            data_buffer[55] <= 0;
            data_buffer[56] <= 0;
            data_buffer[57] <= 0;
            data_buffer[58] <= 0;
            data_buffer[59] <= 0;
            data_buffer[60] <= 0;
            data_buffer[61] <= 0;
            data_buffer[62] <= 0;
            data_buffer[63] <= 0;
            data_buffer[64] <= 0;
            data_buffer[65] <= 0;
            data_buffer[66] <= 0;
            data_buffer[67] <= 0;
            data_buffer[68] <= 0;
            data_buffer[69] <= 0;
            data_buffer[70] <= 0;
            data_buffer[71] <= 0;
            data_buffer[72] <= 0;
            data_buffer[73] <= 0;
            data_buffer[74] <= 0;
            data_buffer[75] <= 0;
            data_buffer[76] <= 0;
            data_buffer[77] <= 0;
            data_buffer[78] <= 0;
            data_buffer[79] <= 0;
            data_buffer[80] <= 0;
            data_buffer[81] <= 0;
            data_buffer[82] <= 0;
            data_buffer[83] <= 0;
            data_buffer[84] <= 0;
            data_buffer[85] <= 0;
            data_buffer[86] <= 0;
            data_buffer[87] <= 0;
            data_buffer[88] <= 0;
            data_buffer[89] <= 0;
            data_buffer[90] <= 0;
            data_buffer[91] <= 0;
            data_buffer[92] <= 0;
            data_buffer[93] <= 0;
            data_buffer[94] <= 0;
            data_buffer[95] <= 0;
            data_buffer[96] <= 0;
            data_buffer[97] <= 0;
            data_buffer[98] <= 0;
            data_buffer[99] <= 0;
            data_buffer[100] <= 0;
            data_buffer[101] <= 0;
            data_buffer[102] <= 0;
            data_buffer[103] <= 0;
            data_buffer[104] <= 0;
            data_buffer[105] <= 0;
            data_buffer[106] <= 0;
            data_buffer[107] <= 0;
            data_buffer[108] <= 0;
            data_buffer[109] <= 0;
            data_buffer[110] <= 0;
            data_buffer[111] <= 0;
            data_buffer[112] <= 0;
            data_buffer[113] <= 0;
            data_buffer[114] <= 0;
            data_buffer[115] <= 0;
            data_buffer[116] <= 0;
            data_buffer[117] <= 0;
            data_buffer[118] <= 0;
            data_buffer[119] <= 0;
            data_buffer[120] <= 0;
            data_buffer[121] <= 0;
            data_buffer[122] <= 0;
            data_buffer[123] <= 0;
            data_buffer[124] <= 0;
            data_buffer[125] <= 0;
            data_buffer[126] <= 0;
            data_buffer[127] <= 0;
            data_buffer[128] <= 0;
            data_buffer[129] <= 0;
            data_buffer[130] <= 0;
            data_buffer[131] <= 0;
            data_buffer[132] <= 0;
            data_buffer[133] <= 0;
            data_buffer[134] <= 0;
            data_buffer[135] <= 0;
            data_buffer[136] <= 0;
            data_buffer[137] <= 0;
            data_buffer[138] <= 0;
            data_buffer[139] <= 0;
            data_buffer[140] <= 0;
            data_buffer[141] <= 0;
            data_buffer[142] <= 0;
            data_buffer[143] <= 0;
            data_buffer[144] <= 0;
            data_buffer[145] <= 0;
            data_buffer[146] <= 0;
            data_buffer[147] <= 0;
            data_buffer[148] <= 0;
            data_buffer[149] <= 0;
            data_buffer[150] <= 0;
            data_buffer[151] <= 0;
            data_buffer[152] <= 0;
            data_buffer[153] <= 0;
            data_buffer[154] <= 0;
            data_buffer[155] <= 0;
            data_buffer[156] <= 0;
            data_buffer[157] <= 0;
            data_buffer[158] <= 0;
            data_buffer[159] <= 0;
            data_buffer[160] <= 0;
            data_buffer[161] <= 0;
            data_buffer[162] <= 0;
            data_buffer[163] <= 0;
            data_buffer[164] <= 0;
            data_buffer[165] <= 0;
            data_buffer[166] <= 0;
            data_buffer[167] <= 0;
        end else begin
            case (state)
                S_IDLE: begin
                    valid <= 0; // Clear valid signal
                    if (encode_en && ready) begin
                        i <= 0;
                        ready <= 0; // Not ready until encoding is done
                        state <= S_LOAD;
                        // Load data into buffer
                        data_buffer[0] <= datain[0*8 +: 8];
                        data_buffer[1] <= datain[1*8 +: 8];
                        data_buffer[2] <= datain[2*8 +: 8];
                        data_buffer[3] <= datain[3*8 +: 8];
                        data_buffer[4] <= datain[4*8 +: 8];
                        data_buffer[5] <= datain[5*8 +: 8];
                        data_buffer[6] <= datain[6*8 +: 8];
                        data_buffer[7] <= datain[7*8 +: 8];
                        data_buffer[8] <= datain[8*8 +: 8];
                        data_buffer[9] <= datain[9*8 +: 8];
                        data_buffer[10] <= datain[10*8 +: 8];
                        data_buffer[11] <= datain[11*8 +: 8];
                        data_buffer[12] <= datain[12*8 +: 8];
                        data_buffer[13] <= datain[13*8 +: 8];
                        data_buffer[14] <= datain[14*8 +: 8];
                        data_buffer[15] <= datain[15*8 +: 8];
                        data_buffer[16] <= datain[16*8 +: 8];
                        data_buffer[17] <= datain[17*8 +: 8];
                        data_buffer[18] <= datain[18*8 +: 8];
                        data_buffer[19] <= datain[19*8 +: 8];
                        data_buffer[20] <= datain[20*8 +: 8];
                        data_buffer[21] <= datain[21*8 +: 8];
                        data_buffer[22] <= datain[22*8 +: 8];
                        data_buffer[23] <= datain[23*8 +: 8];
                        data_buffer[24] <= datain[24*8 +: 8];
                        data_buffer[25] <= datain[25*8 +: 8];
                        data_buffer[26] <= datain[26*8 +: 8];
                        data_buffer[27] <= datain[27*8 +: 8];
                        data_buffer[28] <= datain[28*8 +: 8];
                        data_buffer[29] <= datain[29*8 +: 8];
                        data_buffer[30] <= datain[30*8 +: 8];
                        data_buffer[31] <= datain[31*8 +: 8];
                        data_buffer[32] <= datain[32*8 +: 8];
                        data_buffer[33] <= datain[33*8 +: 8];
                        data_buffer[34] <= datain[34*8 +: 8];
                        data_buffer[35] <= datain[35*8 +: 8];
                        data_buffer[36] <= datain[36*8 +: 8];
                        data_buffer[37] <= datain[37*8 +: 8];
                        data_buffer[38] <= datain[38*8 +: 8];
                        data_buffer[39] <= datain[39*8 +: 8];
                        data_buffer[40] <= datain[40*8 +: 8];
                        data_buffer[41] <= datain[41*8 +: 8];
                        data_buffer[42] <= datain[42*8 +: 8];
                        data_buffer[43] <= datain[43*8 +: 8];
                        data_buffer[44] <= datain[44*8 +: 8];
                        data_buffer[45] <= datain[45*8 +: 8];
                        data_buffer[46] <= datain[46*8 +: 8];
                        data_buffer[47] <= datain[47*8 +: 8];
                        data_buffer[48] <= datain[48*8 +: 8];
                        data_buffer[49] <= datain[49*8 +: 8];
                        data_buffer[50] <= datain[50*8 +: 8];
                        data_buffer[51] <= datain[51*8 +: 8];
                        data_buffer[52] <= datain[52*8 +: 8];
                        data_buffer[53] <= datain[53*8 +: 8];
                        data_buffer[54] <= datain[54*8 +: 8];
                        data_buffer[55] <= datain[55*8 +: 8];
                        data_buffer[56] <= datain[56*8 +: 8];
                        data_buffer[57] <= datain[57*8 +: 8];
                        data_buffer[58] <= datain[58*8 +: 8];
                        data_buffer[59] <= datain[59*8 +: 8];
                        data_buffer[60] <= datain[60*8 +: 8];
                        data_buffer[61] <= datain[61*8 +: 8];
                        data_buffer[62] <= datain[62*8 +: 8];
                        data_buffer[63] <= datain[63*8 +: 8];
                        data_buffer[64] <= datain[64*8 +: 8];
                        data_buffer[65] <= datain[65*8 +: 8];
                        data_buffer[66] <= datain[66*8 +: 8];
                        data_buffer[67] <= datain[67*8 +: 8];
                        data_buffer[68] <= datain[68*8 +: 8];
                        data_buffer[69] <= datain[69*8 +: 8];
                        data_buffer[70] <= datain[70*8 +: 8];
                        data_buffer[71] <= datain[71*8 +: 8];
                        data_buffer[72] <= datain[72*8 +: 8];
                        data_buffer[73] <= datain[73*8 +: 8];
                        data_buffer[74] <= datain[74*8 +: 8];
                        data_buffer[75] <= datain[75*8 +: 8];
                        data_buffer[76] <= datain[76*8 +: 8];
                        data_buffer[77] <= datain[77*8 +: 8];
                        data_buffer[78] <= datain[78*8 +: 8];
                        data_buffer[79] <= datain[79*8 +: 8];
                        data_buffer[80] <= datain[80*8 +: 8];
                        data_buffer[81] <= datain[81*8 +: 8];
                        data_buffer[82] <= datain[82*8 +: 8];
                        data_buffer[83] <= datain[83*8 +: 8];
                        data_buffer[84] <= datain[84*8 +: 8];
                        data_buffer[85] <= datain[85*8 +: 8];
                        data_buffer[86] <= datain[86*8 +: 8];
                        data_buffer[87] <= datain[87*8 +: 8];
                        data_buffer[88] <= datain[88*8 +: 8];
                        data_buffer[89] <= datain[89*8 +: 8];
                        data_buffer[90] <= datain[90*8 +: 8];
                        data_buffer[91] <= datain[91*8 +: 8];
                        data_buffer[92] <= datain[92*8 +: 8];
                        data_buffer[93] <= datain[93*8 +: 8];
                        data_buffer[94] <= datain[94*8 +: 8];
                        data_buffer[95] <= datain[95*8 +: 8];
                        data_buffer[96] <= datain[96*8 +: 8];
                        data_buffer[97] <= datain[97*8 +: 8];
                        data_buffer[98] <= datain[98*8 +: 8];
                        data_buffer[99] <= datain[99*8 +: 8];
                        data_buffer[100] <= datain[100*8 +: 8];
                        data_buffer[101] <= datain[101*8 +: 8];
                        data_buffer[102] <= datain[102*8 +: 8];
                        data_buffer[103] <= datain[103*8 +: 8];
                        data_buffer[104] <= datain[104*8 +: 8];
                        data_buffer[105] <= datain[105*8 +: 8];
                        data_buffer[106] <= datain[106*8 +: 8];
                        data_buffer[107] <= datain[107*8 +: 8];
                        data_buffer[108] <= datain[108*8 +: 8];
                        data_buffer[109] <= datain[109*8 +: 8];
                        data_buffer[110] <= datain[110*8 +: 8];
                        data_buffer[111] <= datain[111*8 +: 8];
                        data_buffer[112] <= datain[112*8 +: 8];
                        data_buffer[113] <= datain[113*8 +: 8];
                        data_buffer[114] <= datain[114*8 +: 8];
                        data_buffer[115] <= datain[115*8 +: 8];
                        data_buffer[116] <= datain[116*8 +: 8];
                        data_buffer[117] <= datain[117*8 +: 8];
                        data_buffer[118] <= datain[118*8 +: 8];
                        data_buffer[119] <= datain[119*8 +: 8];
                        data_buffer[120] <= datain[120*8 +: 8];
                        data_buffer[121] <= datain[121*8 +: 8];
                        data_buffer[122] <= datain[122*8 +: 8];
                        data_buffer[123] <= datain[123*8 +: 8];
                        data_buffer[124] <= datain[124*8 +: 8];
                        data_buffer[125] <= datain[125*8 +: 8];
                        data_buffer[126] <= datain[126*8 +: 8];
                        data_buffer[127] <= datain[127*8 +: 8];
                        data_buffer[128] <= datain[128*8 +: 8];
                        data_buffer[129] <= datain[129*8 +: 8];
                        data_buffer[130] <= datain[130*8 +: 8];
                        data_buffer[131] <= datain[131*8 +: 8];
                        data_buffer[132] <= datain[132*8 +: 8];
                        data_buffer[133] <= datain[133*8 +: 8];
                        data_buffer[134] <= datain[134*8 +: 8];
                        data_buffer[135] <= datain[135*8 +: 8];
                        data_buffer[136] <= datain[136*8 +: 8];
                        data_buffer[137] <= datain[137*8 +: 8];
                        data_buffer[138] <= datain[138*8 +: 8];
                        data_buffer[139] <= datain[139*8 +: 8];
                        data_buffer[140] <= datain[140*8 +: 8];
                        data_buffer[141] <= datain[141*8 +: 8];
                        data_buffer[142] <= datain[142*8 +: 8];
                        data_buffer[143] <= datain[143*8 +: 8];
                        data_buffer[144] <= datain[144*8 +: 8];
                        data_buffer[145] <= datain[145*8 +: 8];
                        data_buffer[146] <= datain[146*8 +: 8];
                        data_buffer[147] <= datain[147*8 +: 8];
                        data_buffer[148] <= datain[148*8 +: 8];
                        data_buffer[149] <= datain[149*8 +: 8];
                        data_buffer[150] <= datain[150*8 +: 8];
                        data_buffer[151] <= datain[151*8 +: 8];
                        data_buffer[152] <= datain[152*8 +: 8];
                        data_buffer[153] <= datain[153*8 +: 8];
                        data_buffer[154] <= datain[154*8 +: 8];
                        data_buffer[155] <= datain[155*8 +: 8];
                        data_buffer[156] <= datain[156*8 +: 8];
                        data_buffer[157] <= datain[157*8 +: 8];
                        data_buffer[158] <= datain[158*8 +: 8];
                        data_buffer[159] <= datain[159*8 +: 8];
                        data_buffer[160] <= datain[160*8 +: 8];
                        data_buffer[161] <= datain[161*8 +: 8];
                        data_buffer[162] <= datain[162*8 +: 8];
                        data_buffer[163] <= datain[163*8 +: 8];
                        data_buffer[164] <= datain[164*8 +: 8];
                        data_buffer[165] <= datain[165*8 +: 8];
                        data_buffer[166] <= datain[166*8 +: 8];
                        data_buffer[167] <= datain[167*8 +: 8];
                    end
                end

                S_LOAD: begin
                    enc_ena <= 1; // Enable encoder for 200 cycles
                    if (i < 168) begin
                        message <= data_buffer[i];
                        data_present <= 1; // Latch data for 168 cycles
                        i <= i + 1;
                        // if (i > 0) begin
                        encoded_data[(i-1)*8 +: 8] <= encoded;
                        // end
                    end else begin
                        encoded_data[(i-1)*8 +: 8] <= encoded;
                        data_present <= 0; // Stop latching data
                        state <= S_ENCODE;
                    end
                end

                S_ENCODE: begin
                    // Wait for encoder to finish encoding
                    if (i >= 200) begin
                        enc_ena <= 0; // Disable encoder
                        // encoded_data[(i)*8 +: 8] <= encoded;
                        state <= S_FINISH; // Move to finish state
                    end else begin
                        i <= i + 1;
                        encoded_data[(i)*8 +: 8] <= encoded;
                    end
                end

                S_FINISH: begin
                    valid <= 1; // Indicate encoding complete
                    ready <= 1; // Ready for next encoding
                    state <= S_IDLE; // Go back to idle state
                end
            endcase
        end
    end

endmodule

