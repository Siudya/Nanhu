module AP0BP1 (    
    input wire clk,        
    input wire rst_n,     
    input wire A,          
    input wire B,          
    output reg C           
);

// 用于保存A和B信号前一个状态的寄存器  
reg A_prev, B_prev;
// 初始化输出C  

  
// 上升沿检测逻辑  
always @(posedge clk or negedge rst_n) begin  
    if (!rst_n) begin  
        // 如果复位信号有效，将C复位到初始状态  
        C <= 0; // 或者 1，取决于你的复位逻辑  
    end else begin  
        // 检查A和B的上升沿  
        if (A && ~A_prev) begin  
            // 当A从0变为1时，将C置为低  
            C <= 0;  
        end else if (B && ~B_prev) begin  
            // 当B从0变为1时，将C置为高  
            C <= 1;  
        end  
    end  
end  
  
  
  
// 在时钟上升沿更新A和B的前一个状态  
always @(posedge clk or negedge rst_n) begin  
    if (!rst_n) begin  
        // 复位时，将A和B的前一个状态都设置为0  
        A_prev <= 0;  
        B_prev <= 0;  
    end else begin  
        // 在每个时钟上升沿，保存A和B的当前状态  
        A_prev <= A;  
        B_prev <= B;  
    end  
end  
  
endmodule


